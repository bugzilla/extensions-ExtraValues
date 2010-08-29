# -*- Mode: perl; indent-tabs-mode: nil -*-
#
# The contents of this file are subject to the Mozilla Public
# License Version 1.1 (the "License"); you may not use this file
# except in compliance with the License. You may obtain a copy of
# the License at http://www.mozilla.org/MPL/
#
# Software distributed under the License is distributed on an "AS
# IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
# implied. See the License for the specific language governing
# rights and limitations under the License.
#
# The Original Code is the ExtraValues Bugzilla Extension.
#
# The Initial Developer of the Original Code is Red Hat, Inc
# Portions created by the Initial Developer are Copyright (C) 2010 the
# Initial Developer. All Rights Reserved.
#
# Contributor(s):
#   Tiago Mello <tmello@everythingsolved.com>

package Bugzilla::Extension::ExtraValues;
use strict;
use base qw(Bugzilla::Extension);

use Bugzilla::Constants;
use Bugzilla::Product;
use Bugzilla::Search;
use Bugzilla::Component;

use Scalar::Util qw(blessed);

our $VERSION = '0.1';

sub install_update_db {
    my ($self, $args) = @_;
    my $dbh = Bugzilla->dbh;

    my $extra_fields = [ 
        { name        => 'cf_extra_components',
          description => 'Extra Components',
          ref_table   => 'components',
          ref_column  => 'name' },

        { name        => 'cf_extra_versions',
          description => 'Extra Versions',
          ref_table   => 'versions',
          ref_column  => 'value' },
    ];

    # Referenced table/column must have the same constraints.
    $dbh->bz_add_index('versions', 'versions_value_idx', [qw(value)]);

    foreach my $extra (@$extra_fields) {
        my $field = new Bugzilla::Field({ name => $extra->{name} });
        next if $field;

        $field = Bugzilla::Field->create({
            name        => $extra->{name},
            description => $extra->{description},
            type        => FIELD_TYPE_MULTI_SELECT, 
            enter_bug   => 1,
            custom      => 1,
        });

        my $bug_cf_table = 'bug_' . $extra->{name};
        my $fk = $dbh->bz_fk_info($bug_cf_table, 'value');
        if ($fk) {
            $fk->{TABLE}  = $extra->{ref_table};
            $fk->{COLUMN} = $extra->{ref_column};
            # 4.1 and above.
            if ($dbh->can('bz_alter_fk')) {
                $dbh->bz_alter_fk($bug_cf_table, 'value', $fk);
            }
            # Before 4.1
            else {
                $dbh->bz_drop_fk($bug_cf_table, 'value');
                $dbh->bz_add_fk($bug_cf_table, 'value', $fk);
            }
        }
    }
}

sub template_before_process {
    my ($self, $args) = @_;
    my ($vars, $file) = @$args{qw(vars file)};

    if ($file eq 'bug/field.html.tmpl') {
        my $field = $vars->{field};
        my $product = blessed $vars->{product} ? $vars->{product}
                                               : $vars->{bug}->product_obj;

        if ($field->name eq 'cf_extra_components') {
            _fix_is_active($product->components);
            $field->{legal_values} = $product->components;
        }
        elsif ($field->name eq 'cf_extra_versions') {
            _fix_is_active($product->versions);
            $field->{legal_values} = $product->versions;
        }
    } 
    elsif ($file =~ m{admin/versions/(confirm-delete|list).html.tmpl}) {
        my $versions = exists $vars->{version} ? [ $vars->{version} ]
                                               : $vars->{product}->versions;
        foreach my $version (@$versions) {
            $version->{bug_count} = _extra_versions_bug_count($version);
        }

    }
    elsif ($file eq 'admin/components/confirm-delete.html.tmpl') {
        $vars->{comp}->{bug_count} =
            _extra_components_bug_count($vars->{comp}, $vars->{product});

    }
    elsif ($file eq 'admin/components/list.html.tmpl') {
        my $product =
            new Bugzilla::Product({ name => $vars->{product}->{name} });

        # In 3.6, {product} is not an object.
        if (blessed $vars->{product}) {
            foreach my $comp (@{$vars->{product}->components}) {
                $comp->{bug_count} = _extra_components_bug_count($comp->{name},
                                                             $product);
            }
        }
    }
    elsif ($file eq 'admin/table.html.tmpl'
        and exists $vars->{custom_fields})
    {
        my @customs;
        foreach my $cf_field (@{$vars->{data}}) {
            if ($cf_field->name !~ m/cf_extra_(components|versions)/) {
                push @customs, $cf_field;
            }
        }
        $vars->{data} = \@customs;
    }
    elsif ($file eq 'admin/custom_fields/cf-js.js.tmpl') {
        my $cache = Bugzilla->request_cache;
        foreach my $extra_field (@{Bugzilla->fields({ is_select => 1 })}) {
            if ($extra_field->name =~ m/cf_extra_(components|versions)/) {
                delete $cache->{fields}->{by_name}->{$extra_field->name};
            }
        }
    }
}

# Implements visibility for Components and Versions in versions before
# 4.2.
sub _fix_is_active {
    my ($values) = @_;
    foreach my $object (@$values) {
        last if $object->can('is_active');
        $object->{is_active} = 1;
    }
    foreach my $object (@$values) {
        last if $object->can('is_visible_on_bug');
        $object->{is_visible_on_bug} = sub { 1 };
    }
}

sub _extra_versions_bug_count {
    my ($version) = @_;
    my $dbh = Bugzilla->dbh;

    my $bug_count = $dbh->selectrow_array(
        qq{
        SELECT COUNT(*) FROM bugs
        WHERE product_id = ?
          AND (version = ? OR 
               bugs.bug_id IN (SELECT bug_id FROM bug_cf_extra_versions
                               WHERE value = ?))}, undef,
        ($version->product_id, $version->name, $version->name)) || 0;

    return $bug_count;
}

sub _extra_components_bug_count {
    my ($comp, $product) = @_;
    my $dbh = Bugzilla->dbh;

    if (!blessed $comp) {
        $comp = Bugzilla::Component->check({ name    => $comp,
                                             product => $product });
    }

    my $bug_count = $dbh->selectrow_array(
        qq{
        SELECT COUNT(*) FROM bugs
        WHERE component_id = ?
          OR bugs.bug_id IN (SELECT bug_id FROM bug_cf_extra_components
                             WHERE value = ?)}, undef,
        ($comp->id, $comp->name)
    );

    return $bug_count;
}

sub object_validators {
    my ($self, $args) = @_;
    my ($class, $validators) = @$args{qw(class validators)};

    if ($class->isa('Bugzilla::Bug')) {
        $class->VALIDATOR_DEPENDENCIES->{cf_extra_components} = ['product'];
        $class->VALIDATOR_DEPENDENCIES->{cf_extra_versions}   = ['product'];

        $validators->{cf_extra_components}
            = sub { _check_cf_extra_components($validators->{component}, @_) };
        $validators->{cf_extra_versions}
            = sub { _check_cf_extra_versions($validators->{version}, @_) };
    }
}

# In 3.6, this is the only way to add new validators.
sub object_before_create {
    my ($self, $args) = @_;
    my ($class, $params) = @$args{qw(class params)};
    # This means that we're in a version that supports object_validators.
    return if $class->can('_get_validators');
    $self->_36_object_validators($class, $params);
}

sub object_before_set {
    my ($self, $args) = @_;
    my $object = $args->{object};
    # This means that we're in a version that supports object_validators.
    return if $object->can('_get_validators');
    $self->_36_object_validators($object);
}

# We don't want to use the code in object_validators for 3.6. 3.6 doesn't
# support VALIDATOR_DEPENDENCIES, and it doesn't have validators in
# VALIDATORS for "component" and "version".
sub _36_object_validators {
    my ($self, $invocant, $params) = @_;
    return unless $invocant->isa('Bugzilla::Bug');
    my $validators = $invocant->VALIDATORS;
    my $product;
    if ($params) {
        $product = $validators->{product}->($invocant, $params->{product});
    }
    $validators->{cf_extra_components} = sub {
        pop @_;
        _check_cf_extra_components('_check_component', @_, $product);
    };
    $validators->{cf_extra_versions} = sub {
        pop @_;
        _check_cf_extra_versions('_check_version', @_, $product);
    };
}

sub _check_cf_extra_components {
    my $original = shift;
    my $invocant = shift;
    my $selected_values = shift;

    my @checked_values;
    foreach my $value (@$selected_values) {
        push @checked_values, $invocant->$original($value, @_)->name;
    }

    return \@checked_values;
}

sub _check_cf_extra_versions {
    my $original = shift;
    my $invocant = shift;
    my $selected_values = shift;

    my @checked_values;
    foreach my $value (@$selected_values) {
        push @checked_values, $invocant->$original($value, @_);
    }

    return \@checked_values;
}

# This only works in 4.0 and above.
sub search_operator_field_override {
    my ($self, $args) = @_;
    my ($class, $operators) = @$args{qw(class operators)};

    $operators->{version} = { _non_changed => \&_extra_version_nonchanged };

    my $component_nonchanged = $operators->{component}->{_non_changed};
    $operators->{component} = {
        _non_changed => sub {
            _extra_component_nonchanged($component_nonchanged, @_);
        }
    }; 
}

sub _extra_version_nonchanged {
    my ($self, $args) = @_;
    my $dbh = Bugzilla->dbh;

    # Version field does not have override function,
    # so first, we want to call direct the operator
    # function from OPERATORS as we do in Search.pm.
    $self->_do_operator_function($args);
    my $term = $args->{term};

    $args->{full_field} = "bug_cf_extra_versions.value";
    $self->_do_operator_function($args);

    $args->{term} = "$term OR "
        . Bugzilla::Search::build_subselect("bugs.bug_id",
        "bug_cf_extra_versions.bug_id",
        "bug_cf_extra_versions", $args->{term});
}

sub _extra_component_nonchanged {
    my $component_nonchanged = shift;
    my ($self, $args) = @_;
    my $dbh = Bugzilla->dbh;

    $self->$component_nonchanged($args);
    my $term = $args->{term};

    $args->{full_field} = "bug_cf_extra_components.value";
    $self->_do_operator_function($args);

    $args->{term} = "$term OR "
        . Bugzilla::Search::build_subselect("bugs.bug_id",
        "bug_cf_extra_components.bug_id",
        "bug_cf_extra_components", $args->{term});
}

__PACKAGE__->NAME;
