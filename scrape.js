var page = require('webpage').create();
                     page.open('http://fantasy.premierleague.com/a/team/13324/event/1', function () {
                     console.log(page.content); //page source
                     phantom.exit();
                     });
