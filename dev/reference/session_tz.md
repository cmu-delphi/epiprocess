# An attempt at a more "accurate"/relevant [`Sys.timezone()`](https://rdrr.io/r/base/timezones.html)

The documentation for
[`Sys.timezone()`](https://rdrr.io/r/base/timezones.html) states that it
tries to detect the "initial" value of the "TZ" environment variable,
but `print` seems to use the current value, so the latter seems more
relevant. It also mentions invalid time zones maybe being treated as
UTC, and maybe being warned about, and `print` on one platform seems to
follow as-UTC, no warning, plus sometimes displaying and sometimes not
displaying the invalid time zone; we don't want to encounter these
situations, so try to abort before they happen. Also, adds a gate for
session timezones of "UTC" used by some CI and cloud computing services,
requiring users to confirm that "UTC" is actually relevant to them.

## Usage

``` r
session_tz()
```
