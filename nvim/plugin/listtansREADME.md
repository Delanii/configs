# listtrans.vim 

Created by [Damian Conway](https://github.com/thoughtstream)

Watch his [video at OSCON](https://www.youtube.com/watch?v=aHm36-na4-4)

This plugin toggles a sentence separated by , or ; to a list.


## Toggle lists

    ;l   :call ListTrans_toggle_format()<CR>
    ;l   :call ListTrans_toggle_format('visual')<CR>

This doesn't work if you are swapping : and ;. Get rid of that.
If you have , in the list and toggle back to a sentence then it will be 
separated by ;.

Toggling from a list to a sentence, it will add 'and' at the end of list item.


## Conjunctions

The default conjuction is 'and'. If you use other conjuctions which listed below, it will be added in the list 
before the last item.

    Default conjunction: and
    Conjunctions: and not, and, plus, with, or else, or otherwise, or, nor, but not, but, else, otherwise



