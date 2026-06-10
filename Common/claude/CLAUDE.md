# Personal Preferences

## Markdown formatting

- **Prefer bulleted lists with a bold lead-in over tables for prose content.**
  Markdown table rows must be single physical lines, which blows past my
  80-column `fill-column` and breaks with `M-q` / `fill-paragraph`. Bulleted
  lists wrap cleanly in raw form and survive Emacs reflow.
- Reserve tables strictly for genuinely tabular data with short cells
  (status grids, short key/value comparisons, numeric data). Never for
  descriptions, prose, or content containing links.
- True Markdown definition lists (`Term\n: def`) are not in CommonMark or GFM
  and render as plain text on GitHub. Avoid that syntax unless the target
  renderer (Pandoc, kramdown, etc.) is known to support it.
- Bullet shape to use:

  ```markdown
  - **Term**: description that can wrap naturally across multiple lines
    indented two spaces under the bullet.
  - **Next term**: another description.
  ```

## Prose style

- **Avoid em dashes (—).** They're an overused LLM tic. Use commas,
  parentheses, colons, or a new sentence instead. This applies everywhere,
  including list separators (use `:`, not `—`, after the bold lead-in).

## C++ style

- **Prefer east const (`Type const*`) over west const (`const Type*`).**
  Write the `const` to the right of what it qualifies, e.g. `std::string const
  *s`, not `const std::string *s`. Apply this to const I introduce in new code;
  don't churn pre-existing west-const declarations on lines I'm not otherwise
  touching, and defer to a project's established convention when its
  instructions say to match surrounding style.
