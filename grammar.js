module.exports = grammar({
    name: "nu",

    word: $ => $.identifier,

    extras: $ => [/\s/, $.comment],

    // conflicts: $ => [
    //     [$.command],
    // ],

    rules: {
        source_file: $ => repeat(choice(
            $._declaration,
            prec(-1, $._statement),
        )),

        identifier: $ => token(/[_\p{XID_Start}][_\p{XID_Continue}]*/),

        cmd_identifier: $ => token(/[_\p{XID_Start}][_\-\p{XID_Continue}]*/),

        _command_name: $ => choice(
            field("unquoted_name", $.cmd_identifier),
            field("quoted_name", $.val_string),
        ),

        _variable_name: $ => choice(
            field("unquoted_name", $.identifier),
            field("dollar_name", $.val_variable),
        ),

        _declaration: $ => choice(
            $.decl_alias,
            $.decl_def,
            $.decl_export,
            $.decl_use,
        ),

        decl_alias: $ => seq(
            optional(MODIFIER().visibility),
            KEYWORD().alias,
            $._command_name,
            PUNCTUATION().equal,
            field("value", $.pipeline),
        ),

        decl_def: $ => seq(
            optional(MODIFIER().visibility),
            choice(KEYWORD().def, KEYWORD().def_env),
            $._command_name,
            field("parameters", choice($.parameter_parens, $.parameter_bracks)),
            field("body", $.block),
        ),

        decl_export: $ => seq(
            KEYWORD().export_env,
            field("body", $.block),
        ),

        decl_use: $ => prec.right(1, seq(
            optional(MODIFIER().visibility),
            KEYWORD().use,
            field("module", $._command_name),
            optional($._scope_pattern),
        )),

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
            field("param_name", $._variable_name),
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
            token.immediate(PUNCTUATION().open_angle),
            field("inner", token(FLAT_TYPES())),
            PUNCTUATION().close_angle,
        )),

        param_cmd: $ => seq(
            PUNCTUATION().at,
            $._command_name,
        ),

        param_rest: $ => token(
            PUNCTUATION().rest,
            token.immediate($.identifier),
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
            $._blosure,
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
                field("catch_branch", $._blosure),
            )),
        ),

        ctrl_return: $ => choice(
            prec(1, seq(KEYWORD().return, $._expression)),
            KEYWORD().return,
        ),

        stmt_let: $ => seq(
            optional(choice(KEYWORD().let, KEYWORD().let_env)),
            $._assignment_pattern,
        ),

        stmt_mut: $ => seq(
            KEYWORD().mut,
            $._assignment_pattern,
        ),

        stmt_const: $ => seq(
            KEYWORD().const,
            $._assignment_pattern,
        ),

        _assignment_pattern: $ => seq(
            $._variable_name,
            PUNCTUATION().equal,
            field("value", $.pipeline),
        ),

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

        _blosure: $ => choice(
            $.block,
            $.val_closure,
        ),

        block: $ => seq(
            PUNCTUATION().open_brace,
            repeat(choice(
                $._declaration,
                $._statement,
            )),
            PUNCTUATION().close_brace,
        ),

        pipeline: $ => seq(
            $.pipe_element,
            repeat(seq(PUNCTUATION().pipe, $.pipe_element))
        ),

        pipe_element: $ => prec(-1, choice(
            prec(3, $._expression),
            $._ctrl_nestable,
            $.command,
        )),

        _expression: $ => choice(
            $._value,
            $.expr_binary,
            $.expr_unary,
            $.expr_parenthesized,
        ),

        expr_unary: $ => seq(
            choice(
                OPR().not,
                OPR().minus,
            ),
            $._expression,
        ),

        expr_binary: $ => choice(
            ...TABLE().map(([precedence, opr]) => prec.left(precedence, seq(
                field("lhs", $._expression),
                field("opr", opr),
                field("rhs", $._expression),
            )))),

        expr_binary_with_unquoted_str: $ => choice(
            ...TABLE().map(([precedence, opr]) => prec.left(precedence, seq(
                field("lhs", $.identifier),
                optional(seq(
                    field("opr", opr),
                    field("rhs", choice($._expression, $.identifier))
                )),
            )))),

        expr_parenthesized: $ => seq(
            PUNCTUATION().open_paren,
            $.pipeline,
            PUNCTUATION().close_paren,
        ),

        _value: $ => choice(
            $.val_variable,
            $.val_nothing,
            $.val_bool,
            $.val_int,
            $.val_float,
            // $.val_range,
            $.val_binary,
            $.val_string,
            $.val_interpolated,
            $.val_filesize,
            $.val_duration,
            $.val_date,
            $.val_list,
            $.val_record,
            $.val_table,
            $.val_closure,
        ),

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
            $._float_literal,
        ),

        _float_literal: $ => {
            const decimal_digits = /\d(_?\d)*/;

            const int_part = choice(
                '0',
                seq(optional('0'), /[1-9]/, optional(seq(optional('_'), decimal_digits)))
            );

            const signed_integer = seq(optional(choice('-', '+')), decimal_digits)
            const exponent_part = seq(choice('e', 'E'), signed_integer)

            return token(choice(
                seq(int_part, '.', optional(decimal_digits), optional(exponent_part)),
                seq(int_part, exponent_part),
            ));
        },

        val_int: $ => token(choice(
            /[+-]?[0-9][0-9_]*/i,
            /0x[0-9a-fA-F_]+/i,
            /0b[01_]+/i,
            /0o[0-7_]+/i,
        )),

        val_binary: $ => {
            const hex_digit = /[0-9a-fA-F_]+/;

            return seq(
                choice("0b[", "0o[", "0x["),
                repeat(seq(hex_digit, optional(PUNCTUATION().comma))),
                PUNCTUATION().close_brack,
            )
        },

        // val_range: $ => {
        //     const opr = choice(
        //         OPR().range_exclusive,
        //         OPR().range_inclusive,
        //         OPR().range_inclusive2,
        //     );
        //
        //     const member = choice($._number, $.expr_parenthesized);
        //
        //     const lo = field("lo", member);
        //     const hi = field("hi", member);
        //
        //     return prec.left(PREC().range, choice(
        //         seq(lo, opr, hi),
        //         seq(lo, opr),
        //         seq(opr, hi),
        //     ));
        // },

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

        _str_single_quotes: $ => seq(
            "'",
            $._unescaped_str_content_single_quote,
            "'",
        ),

        _str_back_ticks: $ => seq(
            "`",
            $._unescaped_str_content_back_tick,
            "`",
        ),

        _unescaped_str_content_single_quote: $ => token.immediate(prec(1, /[^']*/)),

        _unescaped_str_content_back_tick: $ => token.immediate(prec(1, /[^`]*/)),

        _escaped_str_content: $ => token.immediate(prec(1, /[^"\\]+/)),

        escape_sequence: $ => token.immediate(seq(
            "\\",
            choice(
                /[^xu]/,
                /u[0-9a-fA-F]{4}/,
                /u{[0-9a-fA-F]+}/,
                /x[0-9a-fA-F]{2}/
            ),
        )),

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

        val_duration: $ => seq(
            field("value", $._number),
            field("unit", token.immediate(token(DURATION_UNIT()))),
        ),

        val_filesize: $ => seq(
            field("value", $._number),
            field("unit", token.immediate(token(FILESIZE_UNIT()))),
        ),

        _number: $ => choice(
            /[+-]?[0-9][0-9_]*/i,
            $._float_literal
        ),

        val_date: $ => token(
            choice(
                /[0-9]{4}-[0-9]{2}-[0-9]{2}/i,
                /[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]+)?([Zz]|([\+-])([01]\d|2[0-3]):?([0-5]\d)?)?/,
            ),
        ),

        val_list: $ => seq(
            PUNCTUATION().open_brack,
            repeat(field(
                "item",
                seq($._expression, optional(PUNCTUATION().comma)))
            ),
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
            field("parameters", $.parameter_pipes),
            repeat(choice(
                $._statement
            )),
            PUNCTUATION().close_brace,
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

        cell_path: $ => seq(
            $.path,
            repeat($.path),
        ),

        command: $ => prec.right(-1, seq(
            field("head", $.identifier),
            prec.left((PREC().range), optional(choice(
                field("sub", $.identifier),
                alias($.expr_binary_with_unquoted_str, $.expr_binary)
            ))),
            field("args", repeat(choice(
                $.expr_parenthesized,
                $._value,
                $._flag,
                // alias($.identifier, $.val_string),
            ))),
            optional($.redirection),
        )),

        redirection: $ => seq(
            choice(...REDIR()),
            optional($._expression),
        ),

        redirection: $ => choice(
            prec(1, seq(choice(...REDIR()), $._expression)),
            choice(...REDIR()),
        ),

        _flag: $ => choice(
            $.short_flag,
            $._long_flag,
        ),

        short_flag: $ => seq(
            "-",
            token.immediate(/[a-zA-Z0-9]+/),
        ),

        _long_flag: $ => choice(
            field("flag_separator", token("--")),
            field("long_flag", seq($.long_flag, optional($.flag_value))),
        ),

        long_flag: $ => seq(
            "--",
            token.immediate(/[_\p{XID_Start}][_\-\p{XID_Continue}]*/),
        ),

        flag_value: $ => seq(
            PUNCTUATION().equal,
            field("value", choice(
                $.val_string,
                $.identifier,
                /[0-9][0-9_]*/i,
            )),
        ),

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
        "glob", "int", "import-pattern", "keyword", "list", "math", "nothing",
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
