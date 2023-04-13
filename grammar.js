// TODO: 
// - unquoted string might need an external lexer?
module.exports = grammar({
    name: "nu",

    word: $ => $.identifier,

    extras: $ => [/\s/, $.comment],

    externals: $ => [
        $._int_literal,
        $._float_literal,
        $._unquoted_str,
    ],

    conflicts: $ => [
        [$.val_record, $.val_closure]
    ],

    rules: {
        /// File

        nu_script: $ => seq(
            optional($.shebang),
            repeat(choice(
                $._declaration,
                prec(-1, $._statement),
            ))
        ),

        shebang: $ => seq('#!', /.*/),

        /// Identifiers

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
            PUNCTUATION().equal,
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
            PUNCTUATION().open_paren,
            repeat($.parameter),
            PUNCTUATION().close_paren,
        ),

        parameter_bracks: $ => seq(
            PUNCTUATION().open_brack,
            repeat($.parameter),
            PUNCTUATION().close_brack,
        ),

        parameter_pipes: $ => seq(
            PUNCTUATION().pipe,
            repeat($.parameter),
            PUNCTUATION().pipe,
        ),

        parameter: $ => seq(
            $._param_name,
            optional(choice(
                $.param_type,
                $.param_value,
                seq($.param_value, $.param_type),
                seq($.param_type, $.param_value),
            )),
            optional(PUNCTUATION().comma),
        ),

        _param_name: $ => choice(
            field("param_rest", $.param_rest),
            field("param_optional", $.param_opt),
            field("param_name", $.identifier),
            field("param_short_flag", $.param_short_flag),
            $._param_long_flag,
        ),

        param_type: $ => seq(
            PUNCTUATION().colon,
            field("param_type", choice(
                $.list_type,
                FLAT_TYPES(),
            )),
            field("completion", optional($.param_cmd)),
        ),

        param_value: $ => seq(
            PUNCTUATION().equal,
            field("param_value", $._expression),
        ),

        list_type: $ => prec(1, seq(
            "list",
            optional(seq(
                token.immediate(PUNCTUATION().open_angle),
                field("inner", token(FLAT_TYPES())),
                PUNCTUATION().close_angle
            )),
        )),

        param_cmd: $ => seq(
            PUNCTUATION().at,
            $._command_name,
        ),

        param_rest: $ => seq(
            PUNCTUATION().rest,
            $._var_name,
        ),

        param_opt: $ => seq(
            $.identifier,
            token.immediate(PUNCTUATION().question),
        ),

        _param_long_flag: $ => seq(
            field("param_long_flag", seq(
                "--",
                token.immediate(/[_\p{XID_Start}][_\-\p{XID_Continue}]*/),
            )),
            field("flag_capsule", optional($.flag_capsule)),
        ),

        flag_capsule: $ => seq(
            PUNCTUATION().open_paren,
            $.param_short_flag,
            PUNCTUATION().close_paren,
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
            optional(PUNCTUATION().semicolon),
        ),

        /// Control Statements

        _control: $ => choice(
            $._ctrl_standalone,
            $._ctrl_nestable,
        ),

        _ctrl_standalone: $ => choice(
            $.ctrl_for,
            $.ctrl_loop,
            $.ctrl_while,
            $.ctrl_error,
        ),

        _ctrl_nestable: $ => choice(
            field("ctrl_break", KEYWORD().break),
            field("ctrl_continue", KEYWORD().continue),
            $.ctrl_do,
            $.ctrl_if,
            $.ctrl_try,
            $.ctrl_return,
        ),

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
            PUNCTUATION().equal,
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
            PUNCTUATION().open_brack,
            repeat(field("command", $._command_name)),
            PUNCTUATION().close_brack,
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
            PUNCTUATION().open_brace,
            repeat(choice(
                $._declaration,
                $._statement,
            )),
            PUNCTUATION().close_brace,
        ),

        /// Pipeline

        pipeline: $ => prec(-69, seq(
            $.pipe_element,
            repeat(seq(
                PUNCTUATION().pipe,
                $.pipe_element
            )),
        )),

        pipe_element: $ => prec(-1, choice(
            $._expression,
            $._ctrl_nestable,
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
                            token.immediate(PUNCTUATION().open_paren),
                            $.pipeline,
                            PUNCTUATION().close_paren
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
            PUNCTUATION().open_paren,
            optional($.pipeline),
            PUNCTUATION().close_paren,
        ),

        val_range: $ => {
            const opr = choice(
                OPR().range_exclusive,
                OPR().range_inclusive,
                OPR().range_inclusive2,
            );

            const member = choice(
                $.expr_parenthesized,
                $.val_int,
                $.val_float,
            );

            const lo = field("lo", member);
            const hi = field("hi", member);

            return prec.left(PREC().range, choice(
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
            $.val_float,
            $.val_int,
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

        val_float: $ => choice(
            SPECIAL().pos_infinity,
            SPECIAL().neg_infinity,
            SPECIAL().not_a_number,
            seq(
                optional(choice(OPR().minus, OPR().plus)),
                $._float_literal
            ),
        ),

        val_int: $ => choice(
            /0x[0-9a-fA-F_]+/,
            /0b[01_]+/,
            /0o[0-7_]+/,
            seq(
                optional(choice(OPR().minus, OPR().plus)),
                $._int_literal
            ),
        ),

        val_duration: $ => seq(
            field("value", choice($.val_int, $.val_float)),
            field("unit", DURATION_UNIT()),
        ),

        val_filesize: $ => seq(
            field("value", choice($.val_int, $.val_float)),
            field("unit", FILESIZE_UNIT()),
        ),

        val_binary: $ => seq(
            choice("0b[", "0o[", "0x["),
            repeat(seq($.hex_digit, optional(PUNCTUATION().comma))),
            PUNCTUATION().close_brack,
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
            PUNCTUATION().open_paren,
            repeat($._statement),
            PUNCTUATION().close_paren,
        ),

        /// Collections

        val_list: $ => seq(
            PUNCTUATION().open_brack,
            repeat(field(
                "item",
                seq(
                    choice($._expression, alias($.identifier, $.val_string)),
                    optional(PUNCTUATION().comma)
                ),
            )),
            PUNCTUATION().close_brack,
            optional($.cell_path),
        ),

        val_record: $ => seq(
            PUNCTUATION().open_brace,
            repeat(field("entry", $.record_entry)),
            PUNCTUATION().close_brace,
            optional($.cell_path),
        ),

        record_entry: $ => seq(
            field("key", choice($.identifier, $.val_string)),
            PUNCTUATION().colon,
            field("value", $._expression),
            optional(PUNCTUATION().comma),
        ),

        val_table: $ => seq(
            PUNCTUATION().open_brack,
            field("head", seq($.val_list, PUNCTUATION().semicolon)),
            repeat(field("row", $.val_list)),
            PUNCTUATION().close_brack,
            optional($.cell_path),
        ),

        val_closure: $ => seq(
            PUNCTUATION().open_brace,
            field("parameters", optional($.parameter_pipes)),
            repeat(choice(
                $._statement
            )),
            PUNCTUATION().close_brace,
        ),

        /// CellPaths

        cell_path: $ => seq(
            $.path,
            repeat($.path),
        ),

        path: $ => {
            // const quoted = choice(
            //     token($._str_double_quotes),
            //     token($._str_single_quotes),
            //     token($._str_back_ticks),
            // );

            const path = token.immediate(choice(
                /[+-]?[0-9][0-9_]*/i,
                /[_\p{XID_Start}][_\p{XID_Continue}]*/i,
                // quoted,
            ));

            return seq(
                PUNCTUATION().dot,
                choice(
                    field("raw_path", path),
                    field("protected_path", seq(path, PUNCTUATION().question)),
                ),
            )
        },

        /// Commands

        command: $ => prec.right(10, choice(
            $.cmd_head,
            $.cmd_head_sub,
            $.cmd_prefix_head_sub,
        )),

        cmd_head_sub: $ => prec.right(3, seq(
            field("head", $.cmd_identifier),
            field("sub", $.cmd_identifier),
            prec.right(10, repeat($._cmd_arg)),
        )),

        cmd_prefix_head_sub: $ => prec.right(2, seq(
            field("prefix", $.cmd_identifier),
            field("head", $.cmd_identifier),
            field("sub", $.cmd_identifier),
            prec.right(10, repeat($._cmd_arg)),
        )),

        cmd_head: $ => prec.right(1, seq(
            field("head", $.cmd_identifier),
            prec.right(10, repeat($._cmd_arg)),
        )),

        _cmd_arg: $ => choice(
            field("arg", prec(1, $._value)),
            // field("arg_str", alias($._unquoted_str, $.val_string)),
            field("arg_str", alias($.unquoted, $.val_string)),
            field("arg", $.expr_parenthesized),
            field("flag", $._flag),
            field("redir", $.redirection),
        ),

        unquoted: $ => token(/[^\s\n\t\r{}()\[\]"`']+/),

        redirection: $ => choice(
            prec.right(10, seq(
                choice(...REDIR()),
                choice(
                    alias($._unquoted_str, $.val_string),
                    $._expression,
                )
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
            PUNCTUATION().equal,
            field("value", choice(
                $.val_string,
                $.cmd_identifier,
                /[0-9][0-9_]*/i,
            )),
        ),

        /// Comments

        comment: $ => seq(
            PUNCTUATION().hash,
            /.*/,
        )
    },
});

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

function REDIR() {
    return [
        "err>", "out>",
        "e>", "o>",
        "err+out>", "out+err>",
        "o+e>", "e+o>"
    ]
}

function PUNCTUATION() {
    return {
        at: "@",
        dot: ".",
        hash: "#",
        pipe: "|",
        rest: "...",
        equal: "=",
        colon: ":",
        comma: ",",
        dollar: "$",
        fat_arrow: "=>",
        question: "?",
        open_angle: "<",
        open_brack: "[",
        open_brace: "{",
        open_paren: "(",
        close_angle: ">",
        close_brack: "]",
        close_brace: "}",
        close_paren: ")",
        semicolon: ";",
    }
}

function OPR() {
    return {
        plus: "+",
        minus: "-",
        times: "*",
        divide: "/",
        modulo: "mod",
        floor: "//",
        power: "**",
        append: "++",

        equal: "==",
        not_equal: "!=",
        less_than: "<",
        less_than_equal: "<=",
        greater_than: ">",
        greater_than_equal: ">=",
        regex_match: "=~",
        regex_not_match: "!~",

        in: "in",
        not_in: "not-in",
        not: "not",
        and: "and",
        or: "or",
        xor: "xor",

        bit_or: "bit-or",
        bit_xor: "bit-xor",
        bit_and: "bit-and",
        bit_shl: "bit-shl",
        bit_shr: "bit-shr",

        starts_with: "starts-with",
        ends_with: "ends-with",

        assign: "=",
        assign_add: "+=",
        assign_sub: "-=",
        assign_mul: "*=",
        assign_div: "/=",
        assign_append: "++=",

        range_inclusive: "..",
        range_inclusive2: "..=",
        range_exclusive: "..<",
    }
}

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

function DURATION_UNIT() {
    return choice(...["ns", "µs", "us", "ms", "sec", "min", "hr", "day", "wk"])
};

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

function noneOf(...chars) {
    const negatedString = chars.map(c => c === '\\' ? '\\\\' : c).join('');
    return new RegExp('[^' + negatedString + ']');
}
