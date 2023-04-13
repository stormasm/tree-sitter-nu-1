module.exports = grammar({
    name: "nu",

    word: $ => $.identifier,

    extras: $ => [/\s/, $.comment],

    // externals: $ => [
    //     $._int_literal,
    //     $._float_literal,
    // ],

    conflicts: $ => [
        [$.val_record, $.val_closure]
    ],

    rules: {
        /// File

        nu_script: $ => seq(
            optional($.shebang),
            repeat(choice(
                $._declaration,
                // NOTE:
                // in a script, this is not a legal top level item, this is
                // only here to cover `config.nu` and `env.nu` which are what
                // this parser will mostly be used for i think
                prec(-1, $._statement),
            ))
        ),

        shebang: $ => seq('#!', /.*/),

        /// Identifiers
        // NOTE:
        // for simplicity, i used the `rust` definition of an identifier
        // but in `nu` the rule is way more relaxed than this

        cmd_identifier: $ => token(/[_\p{XID_Start}][_\-\p{XID_Continue}]*/),

        identifier: $ => token(/[_\p{XID_Start}][_\p{XID_Continue}]*/),

        _command_name: $ => choice(
            field("unquoted_name", $.cmd_identifier),
            field("quoted_name", $.val_string),
        ),

        _variable_name: $ => choice(
            field("var_name", $._var_name),
            field("dollar_name", $.val_variable),
        ),

        _var_name: $ => token(seq(
            noneOf(...NON_IDENT_CHARS(), '#'),
            repeat(noneOf(...NON_IDENT_CHARS()))
        )),

        /// Top Level Items

        _declaration: $ => choice(
            $.decl_alias,
            $.decl_def,
            $.decl_export,
            $.decl_extern,
            $.decl_use,
        ),

        decl_alias: $ => seq(
            optional(MODIFIER().visibility),
            KEYWORD().alias,
            field("name", $._command_name),
            PUNC().equal,
            field("value", $.pipeline),
        ),

        decl_def: $ => seq(
            optional(MODIFIER().visibility),
            choice(KEYWORD().def, KEYWORD().def_env),
            field("name", $._command_name),
            field("parameters", choice($.parameter_parens, $.parameter_bracks)),
            field("body", $.block),
        ),

        decl_export: $ => seq(
            KEYWORD().export_env,
            field("body", $.block),
        ),

        decl_extern: $ => seq(
            optional(MODIFIER().visibility),
            KEYWORD().extern,
            field("name", $._command_name),
            field("signature", choice($.parameter_parens, $.parameter_bracks)),
        ),

        decl_use: $ => prec.left(1, seq(
            optional(MODIFIER().visibility),
            KEYWORD().use,
            field("module", choice(
                $.val_string,
                $.unquoted,
            )),
            optional($._scope_pattern),
        )),

        /// Parameters

        parameter_parens: $ => seq(
            DELIM().open_paren,
            repeat($.parameter),
            DELIM().close_paren,
        ),

        parameter_bracks: $ => seq(
            DELIM().open_brack,
            repeat($.parameter),
            DELIM().close_brack,
        ),

        parameter_pipes: $ => seq(
            PUNC().pipe,
            repeat($.parameter),
            PUNC().pipe,
        ),

        parameter: $ => seq(
            $._param_name,
            optional(choice(
                $.param_type,
                $.param_value,
                seq($.param_value, $.param_type),
                seq($.param_type, $.param_value),
            )),
            optional(PUNC().comma),
        ),

        _param_name: $ => choice(
            field("param_rest", $.param_rest),
            field("param_optional", $.param_opt),
            field("param_name", $.identifier),
            field("param_short_flag", $.param_short_flag),
            $._param_long_flag,
        ),

        param_type: $ => seq(
            PUNC().colon,
            field("param_type", choice(
                $.list_type,
                FLAT_TYPES(),
            )),
            field("completion", optional($.param_cmd)),
        ),

        param_value: $ => seq(
            PUNC().equal,
            field("param_value", $._expression),
        ),

        list_type: $ => prec(1, seq(
            "list",
            optional(seq(
                token.immediate(DELIM().open_angle),
                field("inner", token(FLAT_TYPES())),
                DELIM().close_angle
            )),
        )),

        param_cmd: $ => seq(
            PUNC().at,
            $._command_name,
        ),

        param_rest: $ => seq(
            PUNC().rest,
            $._var_name,
        ),

        param_opt: $ => seq(
            $.identifier,
            token.immediate(PUNC().question),
        ),

        _param_long_flag: $ => seq(
            field("param_long_flag", seq(
                "--",
                token.immediate(/[_\p{XID_Start}][_\-\p{XID_Continue}]*/),
            )),
            field("flag_capsule", optional($.flag_capsule)),
        ),

        flag_capsule: $ => seq(
            DELIM().open_paren,
            $.param_short_flag,
            DELIM().close_paren,
        ),

        param_short_flag: $ => seq(
            "-",
            token.immediate(/[a-zA-Z0-9]/),
        ),

        /// Statements

        _statement: $ => seq(
            choice(
                $._control,
                $._stmt_hide,
                $._stmt_overlay,
                $.stmt_let,
                $.stmt_mut,
                $.stmt_const,
                $.stmt_register,
                $.stmt_source,
                $.assignment,
                $.pipeline
            ),
            optional(PUNC().semicolon),
        ),

        /// Controls

        _control: $ => choice(
            $._ctrl_statement,
            $._ctrl_expression,
        ),

        // control statements cannot be used in pipeline because they
        // do not return values
        _ctrl_statement: $ => choice(
            $.ctrl_for,
            $.ctrl_loop,
            $.ctrl_while,
            $.ctrl_error,
        ),

        // control expressions *return values and can be used in pipelines
        // 
        // * `break` and `continue` do not return values (yet?) but can be
        // used in pipelines
        _ctrl_expression: $ => choice(
            field("ctrl_break", KEYWORD().break),
            field("ctrl_continue", KEYWORD().continue),
            $.ctrl_do,
            $.ctrl_if,
            $.ctrl_try,
            $.ctrl_match,
            $.ctrl_return,
        ),

        // Standalone Controls

        ctrl_for: $ => seq(
            KEYWORD().for,
            $._variable_name,
            KEYWORD().in,
            $._expression,
            $.block,
        ),

        ctrl_loop: $ => seq(
            KEYWORD().loop,
            $.block,
        ),

        ctrl_error: $ => seq(
            KEYWORD().error,
            MODIFIER().error_make,
            $.val_record,
        ),

        ctrl_while: $ => seq(
            KEYWORD().while,
            field("condition", $._expression),
            field("body", $.block),
        ),

        // Nestable Controls

        ctrl_do: $ => prec.left(-1, seq(
            KEYWORD().do,
            $.val_closure,
            optional($._expression),
        )),

        ctrl_if: $ => seq(
            KEYWORD().if,
            field("condition", $._expression),
            field("then_branch", $.block),
            optional(seq(
                KEYWORD().else,
                choice(
                    field("else_block", $.block),
                    field("else_branch", $.ctrl_if),
                ),
            ))
        ),

        ctrl_match: $ => seq(
            KEYWORD().match,
            field("scrutinee", $._expression),
            DELIM().open_brace,
            repeat($.match_arm),
            optional($.default_arm),
            DELIM().close_brace,
        ),

        match_arm: $ => seq(
            field("pattern", $._expression),
            PUNC().fat_arrow,
            field("expression", $._expression),
            optional(PUNC().comma),
        ),

        default_arm: $ => seq(
            field("default_pattern", PUNC().underscore),
            PUNC().fat_arrow,
            field("expression", $._expression),
            optional(PUNC().comma),
        ),

        ctrl_try: $ => seq(
            KEYWORD().try,
            field("try_branch", $.block),
            optional(seq(
                KEYWORD().catch,
                field("catch_branch", $.val_closure),
            )),
        ),

        ctrl_return: $ => choice(
            prec(1, seq(KEYWORD().return, $._expression)),
            KEYWORD().return,
        ),

        /// Storage statements

        stmt_let: $ => prec.right(1, seq(
            choice(KEYWORD().let, KEYWORD().let_env),
            $._assignment_pattern,
        )),

        stmt_mut: $ => prec.right(1, seq(
            KEYWORD().mut,
            $._assignment_pattern,
        )),

        stmt_const: $ => prec.right(1, seq(
            KEYWORD().const,
            $._assignment_pattern,
        )),

        _assignment_pattern: $ => seq(
            field("name", $._variable_name),
            PUNC().equal,
            field("value", $.pipeline),
        ),

        /// Scope Statements

        stmt_source: $ => seq(
            choice(KEYWORD().source, KEYWORD().source_env),
            field("file", choice($.val_string, $.val_variable)),
        ),

        stmt_register: $ => prec.left(-1, seq(
            KEYWORD().register,
            field("plugin", choice($.val_string, $.val_variable)),
            field("signature", optional($.val_record)),
        )),

        _stmt_hide: $ => choice(
            $.hide_mod,
            $.hide_env,
        ),

        hide_mod: $ => prec.left(-1, seq(
            KEYWORD().hide,
            field("module", $._command_name),
            optional($._scope_pattern),
        )),

        hide_env: $ => seq(
            KEYWORD().hide_env,
            field("variable", $._variable_name),
        ),

        _stmt_overlay: $ => choice(
            $.overlay_hide,
            $.overlay_list,
            $.overlay_new,
            $.overlay_use,
        ),

        overlay_list: $ => seq(
            KEYWORD().overlay,
            MODIFIER().overlay_list,
        ),

        overlay_hide: $ => seq(
            KEYWORD().overlay,
            MODIFIER().overlay_hide,
            field("overlay", $._command_name),
        ),

        overlay_new: $ => seq(
            KEYWORD().overlay,
            MODIFIER().overlay_new,
            $._command_name,
        ),

        overlay_use: $ => seq(
            KEYWORD().overlay,
            MODIFIER().overlay_use,
            field("overlay", $._command_name),
            optional(seq(
                KEYWORD().as,
                field("rename", $._command_name)
            )),
        ),

        _scope_pattern: $ => choice(
            field("wildcard", $.wild_card),
            field("command", $._command_name),
            field("command_list", $.command_list),
        ),

        wild_card: $ => token("*"),

        command_list: $ => seq(
            DELIM().open_brack,
            repeat(field("command", $._command_name)),
            DELIM().close_brack,
        ),

        /// Assignment Statement

        assignment: $ => {
            const opr = [
                OPR().assign,
                OPR().assign_add,
                OPR().assign_sub,
                OPR().assign_mul,
                OPR().assign_div,
                OPR().assign_append,
            ];

            return prec.left(PREC().assignment, seq(
                field("lhs", $.val_variable),
                field("opr", choice(...opr)),
                field("rhs", $.pipeline),
            ));
        },

        /// Block

        block: $ => seq(
            DELIM().open_brace,
            repeat(choice(
                $._declaration,
                $._statement,
            )),
            DELIM().close_brace,
        ),

        /// Pipeline

        pipeline: $ => prec(-69, seq(
            $.pipe_element,
            repeat(seq(
                PUNC().pipe,
                $.pipe_element
            )),
        )),

        pipe_element: $ => prec(-1, choice(
            $._expression,
            $._ctrl_expression,
            $.where_command,
            $.command,
        )),

        where_command: $ => seq(
            KEYWORD().where,
            field(
                "predicate",
                choice(...TABLE().map(([precedence, opr]) => prec.left(precedence, seq(
                    field("lhs", alias($.identifier, $.val_string)),
                    field("opr", opr),
                    field("rhs", choice($._expression, alias($.identifier, $.val_string)))
                )))),
            ),
        ),

        /// Expressions

        _expression: $ => choice(
            $._value,
            $.expr_binary,
            $.expr_unary,
            $.val_range,
            $.expr_parenthesized,
        ),

        /// Composite Expressions

        expr_unary: $ => {
            const bool_or_expr = choice(
                $.val_bool,
                $.expr_parenthesized,
            );

            return choice(
                seq(OPR().not, bool_or_expr),
                seq(
                    OPR().minus,
                    alias(
                        seq(
                            token.immediate(DELIM().open_paren),
                            $.pipeline,
                            DELIM().close_paren
                        ),
                        $.expr_parenthesized
                    )
                ),
            );
        },

        expr_binary: $ => choice(
            ...TABLE().map(([precedence, opr]) => prec.left(precedence, seq(
                field("lhs", $._expression),
                field("opr", opr),
                field("rhs", $._expression),
            ))),
        ),

        expr_parenthesized: $ => seq(
            DELIM().open_paren,
            optional($.pipeline),
            DELIM().close_paren,
        ),

        val_range: $ => {
            const opr = choice(
                OPR().range_exclusive,
                OPR().range_inclusive,
                OPR().range_inclusive2,
            );

            const member = choice(
                $.expr_parenthesized,
                $.val_number,
            );

            const lo = field("lo", member);
            const hi = field("hi", member);

            return prec.right(PREC().range, choice(
                seq(lo, opr, hi),
                seq(lo, opr),
                seq(opr, hi),
            ));
        },

        /// Simple Expressions

        _value: $ => choice(
            $.val_variable,
            $.val_nothing,
            $.val_bool,
            $.val_number,
            $.val_duration,
            $.val_filesize,
            $.val_binary,
            $.val_string,
            $.val_interpolated,
            $.val_date,
            $.val_list,
            $.val_record,
            $.val_table,
            $.val_closure,
        ),

        /// Literals

        val_variable: $ => seq(
            token(/\$[_\p{XID_Start}][_\p{XID_Continue}]*/),
            optional($.cell_path),
        ),

        val_nothing: $ => token(SPECIAL().null),

        val_bool: $ => token(choice(SPECIAL().true, SPECIAL().false)),

        // separating floats from integers does not end well
        // especially when it comes to incorporation with ranges.
        val_number: $ => choice(
            /[+-]?([0-9]*[.])?[0-9]+([eE][-+]?\d+)?/,
            /0x[0-9a-fA-F_]+/,
            /0b[01_]+/,
            /0o[0-7_]+/,
            SPECIAL().pos_infinity,
            SPECIAL().neg_infinity,
            SPECIAL().not_a_number,
        ),

        val_duration: $ => seq(
            field("value", $.val_number),
            field("unit", DURATION_UNIT()),
        ),

        val_filesize: $ => seq(
            field("value", $.val_number),
            field("unit", FILESIZE_UNIT()),
        ),

        val_binary: $ => seq(
            choice("0b[", "0o[", "0x["),
            repeat(seq($.hex_digit, optional(PUNC().comma))),
            DELIM().close_brack,
        ),

        val_date: $ => token(
            choice(
                /[0-9]{4}-[0-9]{2}-[0-9]{2}/i,
                /[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]+)?([Zz]|([\+-])([01]\d|2[0-3]):?([0-5]\d)?)?/,
            ),
        ),

        hex_digit: $ => token(/[0-9a-fA-F]+/), // add support for underscores

        val_string: $ => choice(
            $._str_double_quotes,
            $._str_single_quotes,
            $._str_back_ticks,
        ),

        _str_double_quotes: $ => seq(
            '"',
            repeat(choice(
                $._escaped_str_content,
                $.escape_sequence,
            )),
            '"',
        ),

        _escaped_str_content: $ => token.immediate(prec(1, /[^"\\]+/)),

        _str_single_quotes: $ => /'[^']*'/,

        _str_back_ticks: $ => /`[^`]*`/,

        escape_sequence: $ => token.immediate(seq(
            "\\",
            choice(
                /[^xu]/,
                /u[0-9a-fA-F]{4}/,
                /u{[0-9a-fA-F]+}/,
                /x[0-9a-fA-F]{2}/
            ),
        )),

        /// String Interpolation

        val_interpolated: $ => choice(
            $._inter_single_quotes,
            $._inter_double_quotes,
        ),

        _escaped_interpolated_content: $ => token.immediate(prec(1, /[^"\\(]/)),

        _unescaped_interpolated_content: $ => token.immediate(prec(1, /[^'(]/)),

        _inter_single_quotes: $ => seq(
            "$'",
            repeat(choice(
                field("expr", $.expr_interpolated),
                $._unescaped_interpolated_content,
            )),
            token.immediate("'"),
        ),

        _inter_double_quotes: $ => seq(
            '$"',
            repeat(choice(
                field("expr", $.expr_interpolated),
                $.inter_escape_sequence,
                $._escaped_interpolated_content,
            )),
            token.immediate('"'),
        ),

        inter_escape_sequence: $ => token.immediate(
            seq(
                "\\",
                choice(
                    /[^xu]/,
                    /u[0-9a-fA-F]{4}/,
                    /u{[0-9a-fA-F]+}/,
                    /x[0-9a-fA-F]{2}/,
                    "(",
                ),
            )
        ),

        expr_interpolated: $ => seq(
            DELIM().open_paren,
            repeat($._statement),
            DELIM().close_paren,
        ),

        /// Collections

        val_list: $ => seq(
            DELIM().open_brack,
            repeat(field(
                "item",
                seq(
                    choice($._expression, alias($.identifier, $.val_string)),
                    optional(PUNC().comma)
                ),
            )),
            DELIM().close_brack,
            optional($.cell_path),
        ),

        val_record: $ => seq(
            DELIM().open_brace,
            repeat(field("entry", $.record_entry)),
            DELIM().close_brace,
            optional($.cell_path),
        ),

        record_entry: $ => seq(
            field("key", choice($.identifier, $.val_string)),
            PUNC().colon,
            field("value", $._expression),
            optional(PUNC().comma),
        ),

        val_table: $ => seq(
            DELIM().open_brack,
            field("head", seq($.val_list, PUNC().semicolon)),
            repeat(field("row", $.val_list)),
            DELIM().close_brack,
            optional($.cell_path),
        ),

        // parsing of blocks and closures is combined here
        // because otherwise this would conflict with records
        val_closure: $ => seq(
            DELIM().open_brace,
            field("parameters", optional($.parameter_pipes)),
            repeat(choice(
                $._statement
            )),
            DELIM().close_brace,
        ),

        /// CellPaths

        cell_path: $ => seq(
            $.path,
            repeat($.path),
        ),

        path: $ => {
            const quoted = choice(
                $._str_double_quotes,
                $._str_single_quotes,
                $._str_back_ticks,
            );

            const path = choice(
                /[+-]?[0-9][0-9_]*/i,
                token(/[^\s\n\t\r{}()\[\]"`'\?]+/),
                quoted,
            );

            return seq(
                PUNC().dot,
                choice(
                    field("raw_path", path),
                    field("protected_path", seq(path, PUNC().question)),
                ),
            )
        },

        /// Commands

        command: $ => prec.left(10, choice(
            $.cmd_head,
            $.cmd_head_sub,
            $.cmd_prefix_head_sub,
        )),

        cmd_head_sub: $ => prec.right(3, seq(
            field("head", seq(optional(PUNC().caret), $.cmd_identifier)),
            field("sub", $.cmd_identifier),
            prec.right(10, repeat($._cmd_arg)),
        )),

        cmd_prefix_head_sub: $ => prec.right(2, seq(
            field("prefix", $.cmd_identifier),
            field("head", seq(optional(PUNC().caret), $.cmd_identifier)),
            field("sub", $.cmd_identifier),
            prec.right(10, repeat($._cmd_arg)),
        )),

        cmd_head: $ => prec.right(1, seq(
            field("head", seq(optional(PUNC().caret), $.cmd_identifier)),
            prec.right(10, repeat($._cmd_arg)),
        )),

        _cmd_arg: $ => choice(
            field("arg", prec.right(10, $._value)),
            // lowest precedence to make it a last resort
            field("arg_str", alias($.unquoted, $.val_string)),
            field("arg", $.expr_parenthesized),
            field("flag", $._flag),
            field("redir", $.redirection),
        ),

        redirection: $ => choice(
            prec.right(10, seq(
                choice(...REDIR()),
                field("file_path", choice(
                    alias($.unquoted, $.val_string),
                    $._expression,
                )),
            )),
            ...REDIR(),
        ),

        _flag: $ => choice(
            $.short_flag,
            $._long_flag,
        ),

        short_flag: $ => prec.right(5, token(seq(
            "-",
            token.immediate(/[_\p{XID_Continue}]+/),
        ))),

        _long_flag: $ => choice(
            field("flag_separator", $.flag_separator),
            field("long_flag", seq($.long_flag, optional($.flag_value))),
        ),

        flag_separator: $ => token("--"),

        long_flag: $ => seq(
            "--",
            token.immediate(/[_\p{XID_Continue}]+/),
        ),

        flag_value: $ => seq(
            PUNC().equal,
            field("value", choice(
                $.val_string,
                $.cmd_identifier,
                /[0-9][0-9_]*/i,
            )),
        ),

        // because this catches almost anything, we want to ensure it is
        // picked as the a last resort after everything else has failed. 
        // so we give it a ridiculous precedence and place it at the
        // very end
        unquoted: $ => prec.left(-69, token(/[^\s\n\t\r{}()\[\]"`']+/)),

        /// Comments

        comment: $ => seq(
            PUNC().hash,
            /.*/,
        )
    },
});

// nushell keywords
function KEYWORD() {
    return {
        def: "def",
        def_env: "def-env",
        alias: "alias",
        use: "use",
        export_env: "export-env",
        extern: "extern",

        let: "let",
        let_env: "let-env",
        mut: "mut",
        const: "const",

        hide: "hide",
        hide_env: "hide-env",

        source: "source",
        source_env: "source-env",

        overlay: "overlay",
        register: "register",

        for: "for",
        loop: "loop",
        while: "while",
        error: "error",

        do: "do",
        if: "if",
        else: "else",
        try: "try",
        catch: "catch",
        match: "match",

        break: "break",
        continue: "continue",
        return: "return",

        as: "as",
        in: "in",

        where: "where",
    }
}

// modifier keywords
function MODIFIER() {
    return {
        overlay_hide: "hide",
        overlay_list: "list",
        overlay_new: "new",
        overlay_use: "use",

        error_make: "make",

        visibility: "export",
    }
}

// redirection
function REDIR() {
    return [
        "err>", "out>",
        "e>", "o>",
        "err+out>", "out+err>",
        "o+e>", "e+o>"
    ]
}

// punctuation
function PUNC() {
    return {
        at: "@",
        dot: ".",
        hash: "#",
        pipe: "|",
        rest: "...",
        equal: "=",
        colon: ":",
        comma: ",",
        caret: "^",
        dollar: "$",
        fat_arrow: "=>",
        question: "?",
        underscore: "_",

        semicolon: ";",
    }
}

// delimiters
function DELIM() {
    return {
        open_angle: "<",
        close_angle: ">",

        open_brack: "[",
        close_brack: "]",

        open_brace: "{",
        close_brace: "}",

        open_paren: "(",
        close_paren: ")",
    }
}

// operators
function OPR() {
    return {
        // arithmetic
        plus: "+",
        minus: "-",
        times: "*",
        divide: "/",
        modulo: "mod",
        floor: "//",
        power: "**",
        append: "++",

        // comparison
        equal: "==",
        not_equal: "!=",
        less_than: "<",
        less_than_equal: "<=",
        greater_than: ">",
        greater_than_equal: ">=",

        // regex matching
        regex_match: "=~",
        regex_not_match: "!~",

        // logical
        not: "not",
        and: "and",
        or: "or",
        xor: "xor",

        // bitwise
        bit_or: "bit-or",
        bit_xor: "bit-xor",
        bit_and: "bit-and",
        bit_shl: "bit-shl",
        bit_shr: "bit-shr",

        // membership tests
        in: "in",
        not_in: "not-in",
        starts_with: "starts-with",
        ends_with: "ends-with",

        // assignment
        assign: "=",
        assign_add: "+=",
        assign_sub: "-=",
        assign_mul: "*=",
        assign_div: "/=",
        assign_append: "++=",

        // range
        range_inclusive: "..",
        range_inclusive2: "..=",
        range_exclusive: "..<",
    }
}

/// operator precedence 
/// taken from `nu_protocol::`
function PREC() {
    return {
        range: 15,
        power: 14,
        multiplicative: 13,
        additive: 12,
        bit_shift: 11,
        comparative: 10,
        membership: 9,
        regex: 8,
        bit_and: 7,
        bit_xor: 6,
        bit_or: 5,
        and: 4,
        xor: 3,
        or: 2,
        assignment: 1,
    }
}

/// map of operators and their precedence
function TABLE() {
    const multiplicatives = choice(
        OPR().times,
        OPR().divide,
        OPR().modulo,
        OPR().floor,
    );

    const comparatives = choice(
        OPR().equal,
        OPR().not_equal,
        OPR().less_than,
        OPR().less_than_equal,
        OPR().greater_than,
        OPR().greater_than_equal,
    );

    const memberships = choice(
        OPR().in,
        OPR().not_in,
        OPR().starts_with,
        OPR().ends_with,
    );

    // `range` is not included here and is handled separately
    return [
        [PREC().power, OPR().power],
        [PREC().multiplicative, multiplicatives],
        [PREC().additive, choice(OPR().plus, OPR().minus)],
        [PREC().bit_shift, choice(OPR().bit_shl, OPR().bit_shr)],
        [PREC().comparative, comparatives],
        [PREC().membership, memberships],
        [PREC().regex, choice(OPR().regex_match, OPR().regex_not_match)],
        [PREC().bit_and, OPR().bit_and],
        [PREC().bit_xor, OPR().bit_xor],
        [PREC().bit_or, OPR().bit_or],
        [PREC().and, OPR().and],
        [PREC().xor, OPR().xor],
        [PREC().or, OPR().or],
    ]
}

/// special tokens
function SPECIAL() {
    return {
        true: "true",
        false: "false",
        null: "null",

        pos_infinity: "inf",
        neg_infinity: "-inf",
        not_a_number: "NaN",
    }
}

/// nushell flat types 
/// taken from `nu_parser::parser::parse_shape_name()`
// i.e not composite types like list<int> or record<name: string>
function FLAT_TYPES() {
    const types = [
        "any", "binary", "block", "bool", "cell-path", "closure", "cond",
        "datetime", "directory", "duration", "directory", "duration",
        "error", "expr", "float", "decimal", "filesize", "full-cell-path",
        "glob", "int", "import-pattern", "keyword", "math", "nothing",
        "number", "one-of", "operator", "path", "range", "record", "signature",
        "string", "table", "variable", "var-with-opt-type"
    ];

    return field("flat_type", choice(...types))
}

/// duration units, are case sensitive
/// taken from `nu_parser::parse_duration_bytes()`
function DURATION_UNIT() {
    return choice(...["ns", "µs", "us", "ms", "sec", "min", "hr", "day", "wk"])
};

/// filesize units, are case insensitive
/// taken from `nu_parser::parse_filesize_bytes()`
function FILESIZE_UNIT() {
    return choice(...[
        "b", "B",

        "kb", "kB", "Kb", "KB",
        "mb", "mB", "Mb", "MB",
        "gb", "gB", "Gb", "GB",
        "tb", "tB", "Tb", "TB",
        "pb", "pB", "Pb", "PB",
        "eb", "eB", "Eb", "EB",
        "zb", "zB", "Zb", "ZB",

        "kib", "kiB", "kIB", "kIb", "Kib", "KIb", "KIB",
        "mib", "miB", "mIB", "mIb", "Mib", "MIb", "MIB",
        "gib", "giB", "gIB", "gIb", "Gib", "GIb", "GIB",
        "tib", "tiB", "tIB", "tIb", "Tib", "TIb", "TIB",
        "pib", "piB", "pIB", "pIb", "Pib", "PIb", "PIB",
        "eib", "eiB", "eIB", "eIb", "Eib", "EIb", "EIB",
        "zib", "ziB", "zIB", "zIb", "Zib", "ZIb", "ZIB",
    ])
}

/// from `nu-parser::parser::is_identifier_byte()`
function NON_IDENT_CHARS() {
    return [
        '"', "'", '`', '&',
        '.', '\\[', '\\]',
        '(', ')', '{', '}',
        '*', '^', '/', '=',
        '|', '!', '<', '>',
    ]
}


// stolen from tree-sitter-bash
function noneOf(...chars) {
    const negatedString = chars.map(c => c === '\\' ? '\\\\' : c).join('');
    return new RegExp('[^' + negatedString + ']');
}
