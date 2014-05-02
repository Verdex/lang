
require "parse"
require "lang"

function pass_parser( parser, expected_expr, expr )
    local input = parse.mk_string( expr )
    local suc, res, str = parser( input )

    if not suc then
        print( expr .. ": fail on success" )
        return
    end

    if not( str.index == string.len( expr ) + 1 ) then
        print( expr .. ": fail on str.index" )
        return
    end

    if not lang.eq( expected_expr, res ) then
        print( expr .. ": fail on result" )
        print( lang.stringify_annotate( expected_expr ) )
        return
    end

    print( expr .. ": pass" )
end

function fail_parser( parser, expr )
    local input = parse.mk_string( expr )
    local suc, res, str = parser( input )

    if suc then
        print( expr .. ": fail on success" )
        return
    end

    if not( str.index == 1 ) then
        print( expr .. ": fail on str.index" )
        return
    end

    if res then
        print( expr .. ": fail on result" )
        return
    end

    print( expr .. ": pass" )
end

-- var
pass_parser( parse.get_variable, lang.mk_variable "_variable", "_variable" )
pass_parser( parse.get_variable, lang.mk_variable "_variable1234", "_variable1234" )
pass_parser( parse.get_variable, lang.mk_variable "SomeName", "SomeName" )
pass_parser( parse.get_variable, lang.mk_variable "_1234", "_1234" )
pass_parser( parse.get_variable, lang.mk_variable "some_nameOfVariable43", "some_nameOfVariable43" )
pass_parser( parse.get_variable, lang.mk_variable "blah_", "blah_" )

fail_parser( parse.get_variable, "100" )
fail_parser( parse.get_variable, "999_Var" )
fail_parser( parse.get_variable, "777Var" )
fail_parser( parse.get_variable, "444var_" )
fail_parser( parse.get_variable, "," ) 
fail_parser( parse.get_variable, "." ) 
fail_parser( parse.get_variable, "|blah" )

-- abs
pass_parser( parse.get_abstraction,
             lang.mk_abstraction( lang.mk_variable "a", 
                lang.mk_abstraction( lang.mk_variable "beta",   
                    lang.mk_abstraction( lang.mk_variable "_123",
                        lang.mk_variable "unknown" ) ) ),
             [[\ a . \ beta . \ _123 . unknown]] )


-- app
pass_parser( parse.get_application,
             lang.mk_application( lang.mk_variable "a", lang.mk_variable "a" ),
             [[a a]] )


-- paren
pass_parser( parse.get_paren,
             lang.mk_paren( lang.mk_variable "a" ),
             [[( a )]] )


-- lambda calculus
pass_parser( parse.get_lambdaCalculus,
             lang.mk_application( 
                lang.mk_application( lang.mk_variable "a", lang.mk_variable "b" ),
                lang.mk_variable "c" ),
             [[a b c]] )
