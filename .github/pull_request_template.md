### Checklist

Please:

- [ ] Make sure this PR is against "dev", not "main" (unless this is a release
      PR).
- [ ] Request a review from one of the current main reviewers:
      brookslogan, nmdefries.
- [ ] Makes sure to bump the version number in `DESCRIPTION`. Always increment
      the patch version number (the third number), unless you are making a
      release PR from dev to main, in which case increment the minor version
      number (the second number).
- [ ] Describe changes made in NEWS.md, making sure breaking changes
      (backwards-incompatible changes to the documented interface) are noted.
      Collect the changes under the next release number (e.g. if you are on
      1.7.2, then write your changes under the 1.8 heading).
- [ ] Styling, linting, and documentation checks. Make a PR comment with:
  - `/document` to check the package documentation and fix any issues.
  - `/style` to check the style and fix any issues.
  - `/lint` to check the linting.
  - `/preview-docs` to preview the docs.
  - See Actions GitHub tab to track progress of these commands.
- See [DEVELOPMENT.md](DEVELOPMENT.md) for more information on the development
  process.

### Change explanations for reviewer

### Magic GitHub syntax to mark associated Issue(s) as resolved when this is merged into the default branch

- Resolves #{issue number}
