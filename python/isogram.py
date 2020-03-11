"""
Taken from Exercism's Python track:

Determine if a word or phrase is an isogram:
aka a word or phrase without a repeating letter, 
however spaces and hyphens are allowed to appear multiple times.
"""

from collections import Counter
import typing

def is_isogram(string: str) -> bool:
    s: str = string.replace(" ", "").replace("-", "").lower()
    res: bool = all(v <= 1 for v in Counter(s).values())
    return res