
The file `raw-strings.scm` contains a reader extension that lets Guile accept strings such as

    #R-(quotes and escapes \ can " freely be used " here)-

The string between the `#R` and the `(` is the 'delimiter'. The string ends with a `)` followed by the delimiter. In the example above, the delimiter is `-`. The delimiter can be empty, as in

    #R(put your '\"\\`)

If you prefer something other than `(` and `)` to 'delimit the delimiter' you can configure this with the constants `delim-begin` and `delim-end`.

I hope this is useful to someone!
