define_webjump("hn", "http://news.ycombinator.com");
define_webjump("timeswire", "http://www.nytimes.com/timeswire");

// APIs

define_webjump("graphapi", "https://developers.facebook.com/docs/reference/api/");

// Databases

define_webjump("mysql", "http://dev.mysql.com/doc/refman/5.1/en/index.html");

// Haskell

define_webjump("hoogle", "http://www.haskell.org/hoogle/?hoogle=%s",
               $alternative="http:///www.haskell.org/hoogle/");

// HTML and CSS

define_webjump("bootstrap", "http://twitter.github.com/bootstrap/");
define_webjump("css", "https://developer.mozilla.org/en/CSS/CSS_Reference");
define_webjump("dom", "https://developer.mozilla.org/en/Gecko_DOM_Reference");
define_webjump("html", "https://developer.mozilla.org/en/HTML");

// JavaScript

define_webjump("backbone", "http://documentcloud.github.com/backbone/");
define_webjump("jqapi", "http://jqapi.com");
define_webjump("js", "https://developer.mozilla.org/en/JavaScript/Reference");
define_webjump("requirejs", "http://requirejs.org/docs/api.html");
define_webjump("underscore", "http://documentcloud.github.com/underscore/");

// Python

define_webjump("django", "https://docs.djangoproject.com/en/");
define_webjump("haystack", "http://django-haystack.readthedocs.org/en/latest/index.html");
define_webjump("python", "http://docs.python.org/2/");
define_webjump("python2.5", "http://docs.python.org/release/2.5.4/");
define_webjump("sqlalchemy", "http://docs.sqlalchemy.org/en/rel_0_5/");

// Tools

define_webjump("svn", "http://svnbook.red-bean.com/en/1.7/index.html");

