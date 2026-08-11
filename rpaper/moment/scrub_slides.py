#!/usr/bin/env python3
"""Strip author-identifying content from a rendered Quarto/reveal.js deck.

Fallback for when re-rendering from .qmd with author:"" is not convenient.
Prefer re-rendering: this only scrubs the HTML, not linked assets.

Usage: python3 scrub_slides.py in.html out.html
"""
import re, sys

IDENT = re.compile(r"abdullah|mahmud|statmania|thinkermahmud|cadet|mymensingh",
                   re.IGNORECASE)

def scrub(html):
    # 1. <meta name="author"> / citation_author
    html = re.sub(r'<meta\s+name="(author|citation_author)"[^>]*>', '', html,
                  flags=re.IGNORECASE)
    # 2. Quarto title-slide author name
    html = re.sub(r'(<div class="quarto-title-author-name">)(.*?)(</div>)',
                  r'\1\nAnonymous\n\3', html, flags=re.DOTALL)
    # 3. Footer / any <p> carrying a personal or site link
    def kill_links(m):
        inner = m.group(0)
        return ('<p>Press space or arrow to change slides</p>'
                if IDENT.search(inner) else inner)
    html = re.sub(r'<p>.*?</p>', kill_links, html, flags=re.DOTALL)
    # 4. Any surviving anchor to an identifying domain
    html = re.sub(r'<a\s+href="https?://[^"]*(statmania|thinkermahmud)[^"]*"[^>]*>(.*?)</a>',
                  r'\2', html, flags=re.IGNORECASE | re.DOTALL)
    return html

src, dst = sys.argv[1], sys.argv[2]
out = scrub(open(src, encoding='utf-8').read())
open(dst, 'w', encoding='utf-8').write(out)

leaks = [(i, l.strip()[:90]) for i, l in enumerate(out.splitlines(), 1)
         if IDENT.search(l)]
print(f"wrote {dst}")
print("CLEAN - no identifying strings" if not leaks
      else "REMAINING LEAKS:\n" + "\n".join(f"  line {i}: {t}" for i, t in leaks))
