type Movie = (String, Double, String)

-- movies :: [(String, Double, String)]
movies::[Movie]
movies = [ ("Green Book", 8.3, "Peter Farrelly"), ("Inception", 8.8, "Christopher Nolan"), ("Incredibles 2", 7.7, "Brad Bird"), ("The Dark Knight", 9.0, "Christopher Nolan")]

imdbAtLeast::Double->Movie->Bool
imdbAtLeast d (_,b,_) = b>=d

director::String->Movie->Bool
director s (_,_,c) = s==c

and_ :: (Movie -> Bool) -> (Movie -> Bool) -> Movie -> Bool
and_ f1 f2 m = f1 m && f2 m

or_ :: (Movie -> Bool) -> (Movie -> Bool) -> Movie -> Bool
or_ f1 f2 m = f1 m || f2 m


search :: (Movie -> Bool) -> [Movie] -> [Movie]
search f m = filter f m