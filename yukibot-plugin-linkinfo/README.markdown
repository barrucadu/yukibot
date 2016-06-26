yukibot-plugin-linkinfo
===

Configuration
---

```toml
[plugin.linkinfo]

# The number of links in a single message to display the title for.
num-links = 5

# Enabled handlers. The ordering determines priority, if multiple
# handlers match the same URI.
handlers = [ "imgur", "html" ]

# Handler configuration
[plugin.linkinfo.handler.html]
    # The length at which a page title is truncated.
    max-title-len = 100
```
