{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.List.Split (splitOn)
import GHC.Unicode (isSpace)
import Text.HTML.Parser
import Network.HTTP.Conduit (simpleHttp)
import Data.Text.Lazy.Encoding (decodeUtf8)
import HTMLEntities.Decoder (htmlEncodedText)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import Data.Text.Lazy.Builder (toLazyText)
import Rady

main :: IO ()
main = do
  input <- getContents
  putStrLn (rstrip isSpace input)
  let (prefix:rest) = splitOn "http" (rstrip isSpace input)
  let url = "http" <> concat (intersperse "http" rest)
  textBody <- simpleHttp url
  let tokens = canonicalizeTokens (parseTokensLazy (decodeUtf8 textBody))
  case lparse ogmeta tokens of
      Just (title, description) -> do
          putStrLn (prefix <> "title: " <> Text.unpack title)
          --putStrLn (prefix <> map (const '=') (Text.unpack title))
          putStrLn (prefix <> "desc:" <> reNewline prefix
                              (TextLazy.unpack (toLazyText (htmlEncodedText description))))
      Nothing -> do
          case lparse (onlyTitle) tokens of
              Just title -> do
                  putStrLn (prefix <> "title: " <> Text.unpack title)
                  --putStrLn (prefix <> map (const '=') (Text.unpack title))
              Nothing -> do
                  putStrLn (prefix <> "error: no og:meta or title")

ogmeta :: Shape Token (Text, Text)
ogmeta = can $ inTags "html" $ inTags "head"
           ((tagged "meta" (attrEq "property" "og:title" `Interleave` attr "content"))
            `Interleave`
            (tagged "meta" (attrEq "property" "og:description" `Interleave` attr "content")))
 
onlyTitle :: Shape Token Text
onlyTitle = can (inTags "html" $ inTags "head" $ inTags "title" $ text)

inTags tag match = (open tag Empty `Group` (match `Group` close tag))

open :: TagName -> Shape Attr a -> Shape Token a
open tag apat = Match matchByTag genTag
    where matchByTag (TagOpen t attrs) | (tag == t) = lparse apat attrs
          matchByTag _ = Nothing
          genTag xs = TagOpen tag (generate apat xs)
 
tagged :: TagName -> Shape Attr a -> Shape Token a
tagged tag apat = Match matchByTag genTag
    where matchByTag (TagSelfClose t attrs) | (tag == t) = lparse apat attrs
          matchByTag _ = Nothing
          genTag xs = TagSelfClose tag (generate apat xs)

close :: TagName -> Shape Token ()
close tag = Match matchByTag genTag
    where matchByTag (TagClose t) | (tag == t) = Just ()
          matchByTag _ = Nothing
          genTag () = TagClose tag
 
-- -- tagged :: TagName
-- --        -> Shape Attr a
-- --        -> Shape (Tree Token) b
-- --        -> Shape (Tree Token) (a, b)
-- -- tagged tag apat bpat = Match matchByTag genTag
-- --     where matchByTag (Node (TagOpen t attrs) xs) = if tag == t
-- --                                                   then (,) <$> looseParse apat attrs <*> looseParse bpat xs
-- --                                                   else Nothing
-- --           matchByTag _ = Nothing
-- --           genTag (xs, ys) = Node (TagOpen tag (generate apat xs))
-- --                                  (generate bpat ys)
-- -- 
-- -- taggedC :: TagName
-- --        -> Shape Attr a
-- --        -> Shape (Tree Token) a
-- -- taggedC tag apat = Match matchByTag genTag
-- --     where matchByTag (Node (TagSelfClose t attrs) []) = if tag == t
-- --                                                   then looseParse apat attrs
-- --                                                   else Nothing
-- --           matchByTag _ = Nothing
-- --           genTag xs = Node (TagOpen tag (generate apat xs)) []
-- 
attr :: AttrName -> Shape Attr AttrValue
attr name = Match matchAttr (Attr name)
    where matchAttr (Attr x value) | (name == x) = Just value
          matchAttr _ = Nothing

attrEq :: AttrName -> AttrName -> Shape Attr ()
attrEq name value = Match matchAttr (\_ -> (Attr name value))
    where matchAttr (Attr x value') | (name == x && value == value') = Just ()
          matchAttr _ = Nothing

text :: Shape Token Text
text = Iso Text.concat pure (Star (Match matchText fromText))
    where matchText (ContentText text) = Just text
          matchText (ContentChar char) = Just (Text.singleton char)
          matchText _ = Nothing
          fromText text = ContentText text
-- 
-- 
-- -- text :: Shape (Tree Token) Text
-- -- text = Iso Text.concat pure (Star (Match matchText fromText))
-- --     where matchText (Node (ContentText text) []) = Just text
-- --           matchText (Node (ContentChar char) []) = Just (Text.singleton char)
-- --           matchText _ = Nothing
-- --           fromText text = Node (ContentText text) []
-- 
-- -- Tree (rootLabel, subForest)
-- -- TagOpen !TagName [Attr]	
-- -- TagSelfClose !TagName [Attr]	
-- -- TagClose !TagName	
-- -- ContentText !Text	
-- -- ContentChar !Char	
-- -- Comment !Builder	
-- -- Doctype !Text	
-- 
-- -- rootLabel
-- 
-- 
-- --  https://example.org
-- 
-- 
-- 
-- strip :: (Char -> Bool) -> String -> String
-- strip f = dropWhile f . reverse . dropWhile f . reverse
-- 
-- 
-- domain :: String
-- domain = "https://boxbase.org"
-- 
-- blog_root, site_template, unpublished, entries, root :: FilePath
-- 
-- blog_root     = "/home/cheery/blog"
-- site_template = blog_root </> "template.html"
-- unpublished   = blog_root </> "www/unpublished"
-- entries       = blog_root </> "www/entries"
-- root          = blog_root </> "www"
-- 
-- main2 :: IO ()
-- main2 = do
--     putStrLn "foo"
--     entriesList <- allEntries entries
--     unpublishedList <- allUnpublished unpublished
--     let getPrevNext :: FilePath -> (Maybe String, Maybe String)
--         getPrevNext = let map = prevNextMap entriesList
--                       in \this -> case Map.lookup this map of
--                         Just (p,n) -> let t = pathList root this
--                                       in ( relativeLink t . pathList root <$> p
--                                          , relativeLink t . pathList root <$> n)
--                         Nothing -> (Nothing, Nothing)
--     mapM (\this -> do
--         putStrLn (show this)
--         m <- getModificationTime (this </> "index.md")
--         let state = (this, (show m, getPrevNext this))
--         needsUpdate <- needsUpdateOrNot this state
--         pure ()
--         ) entriesList
--     mapM (\this -> do
--         putStrLn (show this)
--         m <- getModificationTime (this </> "index.md")
--         let state = (this, (show m, (Nothing, Nothing)))
--         needsUpdate <- needsUpdateOrNot this state
--         pure ()
--         --putStrLn (show (parseDateFromPath p))
--         --fo <- getModificationTime (p </> "index.md")
--         --putStrLn (show (utctDay fo)))
--         )
--         unpublishedList
--     pure ()
-- 
-- allEntries :: FilePath -> IO [FilePath]
-- allEntries entries = expandDirectory entries
--                  >>= expandDirectories
--                  >>= expandDirectories
--                  >>= expandDirectories
--                  >>= return . sortBy newestFirstByPath
-- 
-- allUnpublished :: FilePath -> IO [FilePath]
-- allUnpublished unpublished = fmap sort (expandDirectory unpublished)
-- 
-- needsUpdateOrNot :: FilePath
--                  -> ([Char], (String, (Maybe String, Maybe String)))
--                  -> IO Bool
-- needsUpdateOrNot this state = do
--     contents <- fmap pack <$> safeLoadFile (this </> "settings.dhall")
--     settings <- mapM (Dhall.input Dhall.auto) contents
--     let needsUpdate = case settings of
--                         Nothing -> True
--                         Just a -> (state /= a)
--     if needsUpdate
--     then let contents' = pretty (Dhall.embed Dhall.inject state)
--          in writeFile (this </> "settings.dhall") (unpack contents')
--     else pure ()
--     pure needsUpdate
-- 
-- -- | Construct a pathlist from filepath as preparation for an URL
-- -- | eg. pathList www_root path_in_www
-- pathList :: FilePath -> FilePath -> [String]
-- pathList root path = filter (/=P.pathSeparator)
--                  <$> P.splitPath (makeRelative root path)
-- 
-- -- | Construct relative link pointing from source to destination.
-- relativeLink :: [String] -> [String] -> String
-- relativeLink (x:xs) (y:ys) | x == y = relativeLink xs ys
-- relativeLink xs ys = join (intersperse "/" ((xs >>= const [".."]) ++ ys))
-- 
-- 
-- newestFirstByPath :: FilePath -> FilePath -> Ordering
-- newestFirstByPath y x = compare (parseDateFromPath x, y) (parseDateFromPath y, x)
-- 
-- expandDirectories :: [FilePath] -> IO [FilePath]
-- expandDirectories = fmap join . mapM expandDirectory
-- 
-- expandDirectory :: FilePath -> IO [FilePath]
-- expandDirectory path = do
--     dirs <- listDirectory path
--     pure (fmap (path</>) dirs)
-- 
-- -- | Map to locate the previous and next post.
-- type PreviousNextPost = Map FilePath (Maybe FilePath, Maybe FilePath)
-- 
-- prevNextMap :: [FilePath] -> PreviousNextPost
-- prevNextMap xs = Map.fromList $
--     zip xs (zip (drop 1 (map pure xs) ++ [Nothing]) (Nothing : map pure xs))
-- 
-- -- getModificationTime (path </> "index.md")
-- 
-- parseDateFromPath :: FilePath -> Maybe Day --(Integer,Int,Int)
-- parseDateFromPath path = do
--     let dirname0 = P.takeDirectory path
--         dirname1 = P.takeDirectory dirname0
--         dirname2 = P.takeDirectory dirname1
--     day   <- readMaybe (takeBaseName dirname0)
--     month <- parseMonth (takeBaseName dirname1)
--     year  <- readMaybe (takeBaseName dirname2)
--     pure (fromGregorian year month day)
-- 
-- -- makeRelative root post
-- 
-- -- iso8601Show :: forall t. ISO8601 t => t -> String
-- 
-- 
-- -- | Get date marking for today.
-- today :: IO Day --(Integer,Int,Int)
-- today = getCurrentTime >>= return . utctDay --toGregorian . utctDay
-- 
-- monthNames :: [String]
-- monthNames = ["jan", "feb", "mar", "apr", "may", "jun",
--               "jul", "aug", "sep", "oct", "nov", "dec"]
-- 
-- parseMonth :: String -> Maybe Int
-- parseMonth name = (+1) <$> elemIndex name monthNames
-- 
-- showMonth :: Int -> String
-- showMonth month = monthNames !! (month-1)
-- 
-- 
-- parseGMT :: (MonadFail m, ParseTime t) => (String -> m t)
-- parseGMT = parseTimeM True defaultTimeLocale rfc822_gmt
-- 
-- -- | The formatGMT would have produced YYYY-MM-DD :: GMT, so nope.
-- formatGMTDay :: Day -> String
-- formatGMTDay day = formatGMT (UTCTime day 0)
-- 
-- formatGMT :: UTCTime -> String
-- formatGMT = formatTime defaultTimeLocale rfc822_gmt
-- 
-- rfc822_gmt = "%a, %d %b %Y %H:%M:%S GMT"
-- 
-- -- -- Now we get to the main course. Next we'll go through the update.py -script. We'll check out how it is structured and what does it do.
-- -- -- 
-- -- -- from markdown2 import markdown
-- -- -- from bs4 import BeautifulSoup, Tag
-- -- -- import datetime, os
-- -- -- import shutil
-- -- -- import subprocess
-- -- -- import re
-- -- -- 
-- -- -- today = datetime.date.today()
-- -- -- 
-- -- -- now_utc = datetime.datetime.utcnow().strftime("%a, %d %b %Y %H:%M:%S GMT")
-- -- -- The today/now_utc contains datetime.date(2020, 8, 28), 'Fri, 28 Aug 2020 09:43:37 GMT', depending on the day.
-- -- -- 
-- -- -- def datepath(date):
-- -- --     return date.strftime('%Y/%b/%d').lower()
-- -- -- 
-- -- -- def datepath_to_iso(datepath):
-- -- --     return datetime.datetime.strptime(datepath, '%Y/%b/%d') .date().isoformat()
-- -- -- The datepath converts the date object into a date path, eg. '2020/aug/28', the datepath_to_iso converts this string back into an unix date string: '2020-08-28'.
-- -- -- 
-- -- -- Next we have a Post -record, I had already abandoned object oriented programming when I started this blog, therefore the script itself has remained relatively maintainable and easy to explain.
-- -- -- 
-- -- -- It's a path,name,uri,date -record.
-- -- -- 
-- -- -- .path is a path string.
-- -- -- .name is the 2020/aug/28/lolpost, eg. "name".
-- -- -- .uri is the URL of the post. Eg. /entries/2020/aug/28/lolpost.
-- -- -- .date is the date tag for the post, the ISO-formatted string, eg. '2020-08-28'.
-- -- -- We have a way to construct all Post records from the fields.
-- -- 
-- -- -- 23:55 < Rembane> Cheery: System.Directory, System.Environment, hm... and more! :D
-- -- -- 00:02 < xsperry> Cheery, also System.FilePath, and for dates there's time package
-- -- 
-- -- type Soup = ()
-- -- 
-- -- type Path = String
-- -- 
-- -- data Post = Post { postPath :: Path     -- eg. entries/2020/aug/28/lolpost
-- --                  , postName :: String   -- eg. 2020/aug/28/lolpost
-- --                  , postUri  :: String   -- eg. /entries/2020/aug/28/lolpost
-- --                  , postDate :: String } -- eg. 2020-08-28
-- --                 -- postTitle :: String
-- -- 
-- -- -- Here's a way to convert the .md into a beautiful soup. Then we have bit of a way to make anything into a beautiful soup. And when the soup doesn't taste well, we have a way to dump it.
-- --
-- -- -- Next comes the configuration. We also got the blog sitemap write starting up here. Then we get a list of all the entries and unpublished posts.
-- -- 
-- -- -- postname = 'what-is-this'
-- -- -- entry = os.path.join(entries, datepath(today), postname)
-- -- -- blog_sitemap = open('www/blog_sitemap.txt', 'w')
-- -- -- blog_sitemap.write(domain + u"/".encode('utf-8') + "\n")
-- -- 
-- -- -- posts.sort(key=lambda post: post.date)
-- -- -- The pre_blocks are reformatted. Each line is converted into a <code> -block and receives a line number through CSS-styling.
-- -- -- 
-- -- formatPreBlocks :: Soup -> Soup
-- -- formatPreBlocks element = _
-- -- -- def format_pre_blocks(html, element):
-- -- --     for pre in element.find_all("pre"):
-- -- --         text = pre.text.rstrip("\n ")
-- -- --         pre.clear()
-- -- --         code = html.new_tag('code')
-- -- --         pre.append(code)
-- -- --         for line in re.split(r"(\n)", text):
-- -- --             if line.isspace():
-- -- --                 code.append(line)
-- -- --                 code = html.new_tag('code')
-- -- --                 pre.append(code)
-- -- --             else:
-- -- --                 code.append(line)
-- -- -- At this point all published posts are formatted and a HTML is generated for them.
-- -- -- 
-- -- -- # All published posts
-- -- -- for i, post in enumerate(posts):
-- -- --     index_path = os.path.join(post.path, 'index.md')
-- -- --     html_path  = os.path.join(post.path, 'index.html')
-- -- -- 
-- -- --     html = soup(site_template, 'lxml')
-- -- --     html.article.contents = markdown_soup(index_path)
-- -- --     set_uri(html, 'prev', get_uri(i-1))
-- -- --     set_uri(html, 'next', get_uri(i+1))
-- -- --     post.title = title = html.article.h1.string
-- -- --     html.title.string += ": " + title
-- -- --     format_pre_blocks(html, html.article)
-- -- --     dump_soup(html_path, html)
-- -- -- 
-- -- --     # Add into sitemap. This is rudimentary measure.
-- -- --     if isinstance(post.uri, unicode):
-- -- --         blog_sitemap.write(domain + post.uri.encode('utf-8') + "\n")
-- -- --     else:
-- -- --         blog_sitemap.write(domain + post.uri + "\n")
-- -- -- The unpublished posts have their own setup.
-- -- -- 
-- -- -- # Rebuild all unpublished posts separately, they require
-- -- -- # bit different handling.
-- -- -- for post in unpublished_posts:
-- -- --     index_path = os.path.join(post.path, 'index.md')
-- -- --     html_path  = os.path.join(post.path, 'index.html')
-- -- -- 
-- -- --     html = soup(site_template, 'lxml')
-- -- --     html.article.contents = markdown_soup(index_path)
-- -- --     set_uri(html, 'prev', get_uri(len(posts)-1))
-- -- --     set_uri(html, 'next', None)
-- -- --     if html.article.h1:
-- -- --         post.title = title = html.article.h1.string
-- -- --         html.title.string += ": " + title
-- -- --     format_pre_blocks(html, html.article)
-- -- --     dump_soup(html_path, html)
-- -- -- The main site is built, A header is added and navigation is dumped to the front with all the posts dumped along with a simple date tag.
-- -- -- 
-- -- -- #os.makedirs(entry)
-- -- -- 
-- -- -- html = soup(site_template, 'lxml')
-- -- -- footer = html.footer
-- -- -- html.body.article.clear()
-- -- -- 
-- -- -- h1 = html.new_tag('h1')
-- -- -- h1.string = "Boxbase - Index"
-- -- -- html.body.article.append(h1)
-- -- -- 
-- -- -- html.body.find("nav", {'id':'article-nav'}).extract()
-- -- -- #nav = html.new_tag('nav')
-- -- -- #html.body.article.append(nav)
-- -- -- 
-- -- -- table = html.new_tag('table')
-- -- -- html.body.article.append(table)
-- -- -- 
-- -- -- for post in reversed(posts):
-- -- --     link = html.new_tag('a')
-- -- --     link['href'] = post.uri
-- -- --     link.string = post.title
-- -- -- 
-- -- --     row = html.new_tag('tr')
-- -- --     col = html.new_tag('td')
-- -- --     date = html.new_tag('time')
-- -- --     date.string = post.date
-- -- --     col.append(date)
-- -- --     row.append(col)
-- -- --     col = html.new_tag('td')
-- -- --     col.append(link)
-- -- --     row.append(col)
-- -- --     table.append(row)
-- -- -- 
-- -- -- #html.body.append(footer)
-- -- -- 
-- -- -- dump_soup(os.path.join(root, 'index.html'), html)
-- -- -- The RSS is written down in a similar way as the index was written, except that only about 10 posts are listed.
-- -- 
-- -- tag :: String -> [(String, String)] -> [Soup] -> Soup
-- -- tag name attrs body = _
-- -- 
-- -- text :: String -> Soup
-- -- text = _
-- -- 
-- -- 
-- -- makePost post summary =
-- --   tag "item" []
-- --   (  tag "title" [] (text (postTitle post))
-- --   <> tag "link" [] (text (domain <> postUri post))
-- --   <> tag "guid" [("isPermaLink", "true")] (text (domain <> postUri post))
-- --   -- <> tag "pubDate" [] (text "")
-- --   -- pd.string = datetime.datetime.strptime(post.date, "%Y-%m-%d").strftime("%a, %d %b %Y %H:%M:%S GMT")
-- --   <> case summary of
-- --        Nothing -> mempty
-- --        Just summaryText -> tag "description" [] (text summaryText)
-- --   )
-- -- 
-- -- take10Last :: [Post] -> [Post]
-- -- take10Last = take 10 . reverse
-- -- 
-- -- 
-- -- loadTitle :: [Post] -> IO [(Post, String)]
-- -- loadTitle posts = _
-- -- 
-- -- loadSummaries :: [Post] -> IO [(Post, Maybe String)]
-- -- loadSummaries posts = _
-- -- 
-- -- -- Do bunch of parallel modifications to a template, then print that template out.
-- -- --
-- -- -- loadXmlTemplate "template.rss"
-- -- --
-- -- -- "link" `replaceContents` text (domain <> "/")
-- -- --
-- -- -- "channel bottom" `replaceContents`
-- -- -- (  tag "lastBuildDate" [] now_utc
-- -- -- <> tag "pubDate" [] now_utc
-- -- -- ...
-- -- 
-- -- foo posts = fmap (fmap (uncurry makePost)) (fmap loadSummaries (take10Last posts))
-- -- 
-- -- -- 'feed.rss'
-- -- -- dump_soup(os.path.join(root, 'feed.rss'), xml)
-- -- 
-- -- 
-- -- -- <?xml version="1.0" encoding="UTF-8"?>
-- -- -- <rss version="2.0">
-- -- --   <channel>
-- -- --     <title>Boxbase</title>
-- -- --     <link></link>
-- -- --     <description>Blog about programming, games.</description>
-- -- --   </channel>
-- -- -- </rss>
-- -- foob :: String
-- -- foob = [xmlHeader, rssFeed _ []] >>= stringify
-- 
-- ---- xmlHeader :: Node
-- ---- xmlHeader = Raw "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
-- ---- 
-- ---- rssFeed :: UTCTime -> [(String, (String, (Maybe String, Day)))] -> Node
-- ---- rssFeed pubDate items = let
-- ----     pubDateText = formatGMT pubDate
-- ----     in Element "rss" [("version", "2.0")]
-- ----     [ Element "channel" []
-- ----       ([ Element "title" [] [CData "Boxbase"]
-- ----       , Element "link" [] [CData "https://boxbase.org/"]
-- ----       , Element "description" [] [CData "Blog about programming."]
-- ----       , Element "lastBuildDate" [] [CData pubDateText]
-- ----       , Element "pubDate" [] [CData pubDateText] ] <> map rssItem items) ]
-- ---- 
-- ---- --          title,   link,    description,  lastBuildDate
-- ---- rssItem :: (String, (String, (Maybe String, Day))) -> Node
-- ---- rssItem cell = Element "item" []
-- ----     ([ Element "title" [] [CData (fst cell)], Element "link" [] [CData (fst (snd cell))] ]
-- ----     <> (case (fst (snd (snd cell))) of
-- ----            Just descr -> [Element "description" [] [CData descr]]
-- ----            Nothing -> [])
-- ----     <> [ Element "guid" [("isPermaLink", "true")] [CData (fst (snd cell))]
-- ----        , Element "pubDate" [] [CData (formatGMTDay (snd (snd (snd cell))))] ])
-- 
-- -- -- <?xml version="1.0" encoding="utf-8"?>
-- -- -- <rss version="2.0">
-- -- -- <channel>
-- -- -- <title>Boxbase</title>
-- -- -- <link>https://boxbase.org/</link>
-- -- -- <description>Blog about programming, games.</description>
-- -- -- <lastBuildDate>Thu, 24 Sep 2020 11:10:25 GMT</lastBuildDate>
-- -- <pubDate>Thu, 24 Sep 2020 11:10:25 GMT</pubDate>
-- -- <item><title>Don't clone that GUI (how about this instead?)</title>
-- -- <link>https://boxbase.org/entries/2020/sep/23/dont-clone-that-gui</link>
-- -- <description>Why it's a bad idea to clone an existing GUI to your program,
-- -- while removing the previous one.  I also illustrate what kind of tricks could
-- -- be used to really correct a GUI.</description>
-- -- <guid isPermaLink="true">https://boxbase.org/entries/2020/sep/23/dont-clone-that-gui</guid>
-- -- <pubDate>Wed, 23 Sep 2020 00:00:00 GMT</pubDate></item>
-- -- <item><title>GPL or not?</title><link>https://boxbase.org/entries/2020/sep/19/gpl-or-not</link><description>There's always this question and it arises again every once a while. I think it's better to use BSD license because it's more welcoming to users.</description><guid isPermaLink="true">https://boxbase.org/entries/2020/sep/19/gpl-or-not</guid><pubDate>Sat, 19 Sep 2020 00:00:00 GMT</pubDate></item><item><title>Medi Madelen Gwosdz' OOP take</title><link>https://boxbase.org/entries/2020/sep/3/medi-madelen-gwosdz-oop-post</link><description>I saw a fairly nice take on OOP again, and liked to feature it in my blog.</description><guid isPermaLink="true">https://boxbase.org/entries/2020/sep/3/medi-madelen-gwosdz-oop-post</guid><pubDate>Thu, 03 Sep 2020 00:00:00 GMT</pubDate></item><item><title>The internals of this blog need an update</title><link>https://boxbase.org/entries/2020/aug/29/blog-internals</link><description>It's time to take the blog internals inside-out. I need a blog platform to have it evolve, and thought about not outright conjuring it up myself.</description><guid isPermaLink="true">https://boxbase.org/entries/2020/aug/29/blog-internals</guid><pubDate>Sat, 29 Aug 2020 00:00:00 GMT</pubDate></item><item><title>Should Purescript's Partial instead be called Unsafe?</title><link>https://boxbase.org/entries/2020/aug/28/should-purescript-partial-be-unsafe</link><description>Purescript's Partial -typeclass may differ from the usual definition of partial functions that is assumed to be observable. There could be several ways how this resolves.</description><guid isPermaLink="true">https://boxbase.org/entries/2020/aug/28/should-purescript-partial-be-unsafe</guid><pubDate>Fri, 28 Aug 2020 00:00:00 GMT</pubDate></item><item><title>Bartosz Milewski's neat defunctionalization talk</title><link>https://boxbase.org/entries/2020/aug/24/bartosz-milewski-replacing-functions-with-data-talk</link><description>Bartosz kept a cool talk just recently in Haskell love conference. It's about defunctionalizing recursive functions.</description><guid isPermaLink="true">https://boxbase.org/entries/2020/aug/24/bartosz-milewski-replacing-functions-with-data-talk</guid><pubDate>Mon, 24 Aug 2020 00:00:00 GMT</pubDate></item><item><title>How to offend a skilled blog author with your blogwriting</title><link>https://boxbase.org/entries/2020/aug/17/how-to-offend-skilled-blog-author-with-your-blogwriting</link><description>Best and neatest advice to help you not do the worst mistakes, from a great wordsmith. I'll make more of these later.</description><guid isPermaLink="true">https://boxbase.org/entries/2020/aug/17/how-to-offend-skilled-blog-author-with-your-blogwriting</guid><pubDate>Mon, 17 Aug 2020 00:00:00 GMT</pubDate></item><item><title>I just removed Google Analytics from this website</title><link>https://boxbase.org/entries/2020/aug/11/removed-google-analytics-from-this-blog</link><description>Quick update letting you know that onwards of today, there's no longer Google Analytics on this website.</description><guid isPermaLink="true">https://boxbase.org/entries/2020/aug/11/removed-google-analytics-from-this-blog</guid><pubDate>Tue, 11 Aug 2020 00:00:00 GMT</pubDate></item><item><title>Review of "Simple Essence of Algebraic Subtyping"</title><link>https://boxbase.org/entries/2020/aug/10/review-of-simple-essence-of-algebraic-subtyping</link><description>MLsub has been picked up again. Lionel Parreaux wrote a paper that I'm examining in this post.</description><guid isPermaLink="true">https://boxbase.org/entries/2020/aug/10/review-of-simple-essence-of-algebraic-subtyping</guid><pubDate>Mon, 10 Aug 2020 00:00:00 GMT</pubDate></item><item><title>How a Haskell programmer wrote a tris in Purescript</title><link>https://boxbase.org/entries/2020/aug/5/how-a-haskell-programmer-wrote-a-tris-in-haskell</link><description>Tried out purescript for a first time, had an excuse.</description><guid isPermaLink="true">https://boxbase.org/entries/2020/aug/5/how-a-haskell-programmer-wrote-a-tris-in-haskell</guid><pubDate>Wed, 05 Aug 2020 00:00:00 GMT</pubDate></item></channel>
-- -- 
-- -- 
-- -- 
-- -- -- Query a       - routine to select or pinpoint something, the 'a' describes what the item pinpoints, were it soup, attribute etc...
-- -- 
-- -- -- Query a -> (a -> a) -> Mutation a
-- -- --
-- -- -- Mutation retrieves a log of what happened.
-- -- --   Mutation overlaps with another mutation (queries overlap).
-- -- --   Operation fails for some other reason.
-- -- --   Operation was supposed to be done once, but it happened multiple times.
-- -- --
-- -- --
-- -- -- Query a -> Fetch a
-- -- --            Fetch (Maybe a)
-- -- --            Fetch [a]
-- -- --
-- -- -- Fetch a -> Fetch b -> Fetch (a,b)
-- -- 
-- -- 
-- -- -- data Query a = Find (a -> Bool)
-- -- 
-- -- -- Find ((== "foo") . tag)
-- -- 
-- -- -- Query a = Find (a -> Bool)
-- -- 
-- -- 
-- -- -- Finally the I run the RSYNC via SSH. Oh and the sitemap is being completed by closing the file handle.
-- -- -- 
-- -- --
-- -- -- https://hackage.haskell.org/package/process-1.0.1.3/docs/System-Process.html
-- -- -- https://hackage.haskell.org/package/typed-process
-- -- -- def upload(src):
-- -- --     dst = 'lol@boxbase:'+src
-- -- --     if os.path.isdir(src):
-- -- --         src += '/'
-- -- --     subprocess.call(['rsync', '-r', '--delete', src, dst])
-- -- -- 
-- -- -- # The sitemap has been completed at this point.
-- -- -- blog_sitemap.close()
-- -- -- 
-- -- -- upload(os.path.join(root, 'index.html'))
-- -- -- upload(os.path.join(root, 'blog_sitemap.txt'))
-- -- -- upload(os.path.join(root, 'feed.rss'))
-- -- -- upload(os.path.join(root, 'style'))
-- -- -- upload(os.path.join(root, 'catalog'))
-- -- -- #upload(os.path.join(root, 'lib'))
-- -- -- upload(unpublished)
-- -- -- upload(entries)
-- -- -- That's it. 🌼 It's Done!
-- 
-- -- writeFile :: FilePath -> String -> IO ()
-- 
rstrip :: (Char -> Bool) -> String -> String
rstrip f = reverse . dropWhile f . reverse

reNewline prefix = (concat
                   . intersperse ("\n" <> prefix)
                   . splitOn "\n")
