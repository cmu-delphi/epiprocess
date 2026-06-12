# as_epi_df and as_epi_archive handle auto-detection and pivoting

    Code
      df <- as_epi_df(raw)
    Message
      Pivoting `value` to wide format using `signal` values as column names: `cases` and `deaths`.
      > To keep long format (`signal` added to `other_keys`), pass `signal_format = "long"`.
      > To skip signal processing, pass `signal_format = "none"`.

---

    Code
      arch <- as_epi_archive(raw_arch)
    Message
      Pivoting `value` to wide format using `signal` values as column names: `cases` and `deaths`.
      > To keep long format (`signal` added to `other_keys`), pass `signal_format = "long"`.
      > To skip signal processing, pass `signal_format = "none"`.

---

    Code
      edf <- as_epi_df(raw_extra)
    Message
      Pivoting `value` to wide format using `signal` values as column names: `cases` and `deaths`.
      > To keep long format (`signal` added to `other_keys`), pass `signal_format = "long"`.
      > To skip signal processing, pass `signal_format = "none"`.

---

    Code
      arch_locf <- as_epi_archive(tib)
    Message
      Pivoting `value` to wide format using `signal` values as column names: `a` and `b`.
      > To keep long format (`signal` added to `other_keys`), pass `signal_format = "long"`.
      > To skip signal processing, pass `signal_format = "none"`.

---

    Code
      invisible(as_epi_df(raw[1:5, ]))
    Message
      Keeping this data in "long" format, with `signal` and `value` columns.
      > To convert to wide format with a(n) `cases` column instead, pass `signal_format = "wide"` instead.
      > Silence with `signal_format = "long"`
      Adding `signal` to `other_keys`.

# Explicit signal formats (long/wide) and guessing

    Code
      df_long <- as_epi_df(raw, signal_format = "long", signal_var = "custom_signal")
    Message
      Adding `custom_signal` to `other_keys`.

---

    Code
      df_long_guess <- as_epi_df(raw_guess, signal_format = "long")
    Message
      Adding `signal` to `other_keys`.

---

    Code
      df_wide <- as_epi_df(raw, signal_format = "wide", signal_var = "custom_signal")

---

    Code
      df_wide_guess <- as_epi_df(raw_guess, signal_format = "wide")

