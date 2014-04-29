
require "parse"

function test_get_variable( var_name )
    local input = parse.mk_string( var_name )
    local suc, res, str = parse.get_variable( input )

    if not suc then
        print( var_name .. ": fail on success" )
        return
    end

    if not( str.index == string.len( var_name ) + 1 ) then
        print( var_name .. ": fail on str.index" )
        return
    end

    if not( res == var_name ) then
        print( var_name .. ": fail on result" )
        return
    end

    print( var_name .. ": pass" )
end

function fail_get_variable( var_name )
    local input = parse.mk_string( var_name )
    local suc, res, str = parse.get_variable( input )

    if suc then
        print( var_name .. ": fail on success" )
        return
    end

    if not( str.index == 1 ) then
        print( var_name .. ": fail on str.index" )
        return
    end

    if res then
        print( var_name .. ": fail on result" )
        return
    end

    print( var_name .. ": pass" )
end

test_get_variable "_variable"
test_get_variable "_variable1234"
test_get_variable "SomeName"
test_get_variable "_1234"
test_get_variable "some_nameOfVariable43"
test_get_variable "blah_"

fail_get_variable "100"
fail_get_variable "999_Var"
fail_get_variable "777Var"
fail_get_variable "444var_"
fail_get_variable ","
fail_get_variable "."
fail_get_variable "|blah"
