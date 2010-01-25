
% Records used for etcher testing
-record(blog, {
            name,
            author,
            posts = []              % [#post{}]
            }).

-record(post, {
            title,
            date,
            comments = [],           % [#comment{}]
            previous,                % undefined | #post{}
            next                     % undefined | #post{}
            }).

-record(comment, {
            author,
            opinion,
            replies                 % [#comment{}]
            }).

-record(chapter, {
            sections                % [#section{}]
            }).

-record(section, {
            title
            }).

-record(person, {
            first_name,
            last_name,
            age,
            gender
            }).

