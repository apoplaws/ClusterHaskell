import Prelude
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.Ord
import Data.Complex

data Direction = Clockwise | CounterClockwise
instance Eq Direction where
    (==)  Clockwise Clockwise = True
    (==)  CounterClockwise CounterClockwise = True
    (==) _ _ = False

det2 :: ((Double, Double), (Double, Double))->Double
det2 ((x11, x12), (x21, x22)) = x11*x22-x12*x21
 
det3 :: ((Double, Double, Double),(Double, Double, Double), (Double, Double, Double))->Double
det3 ((x11, x12, x13), (x21, x22, x23), (x31, x32, x33)) = 
    x11*x22*x33 + x12*x23*x31 + x21*x32*x13 - x31*x22*x13-x21*x12*x33-x31*x13*x11


lin2slv :: ((Double, Double, Double), (Double, Double, Double)) -> (Double, Double)   
lin2slv  ((a11, a12, a1), (a21, a22, a2)) = (w1/w, w2/w) where
    w  = det2((a11, a12), (a21, a22))
    w1 = det2((a1, a12), (a2, a22))
    w2 = det2((a11, a1), (a21, a2))
    
lin3slv :: ((Double, Double, Double, Double),(Double, Double, Double, Double), (Double, Double, Double, Double))->(Double, Double, Double)    
lin3slv  ((a11, a12, a13, a1), (a21, a22, a23, a2), (a31, a32, a33, a3)) = 
    (w1/w, w2/w, w3/w) where
    w   = det3((a11, a12, a13), (a21, a22, a23), (a31, a32, a33))
    w1  = det3((a1,  a12, a13), (a2,  a22, a23), (a3,  a32, a33))
    w2  = det3((a11, a1,  a13), (a21, a2,  a23), (a31, a3,  a33))
    w3  = det3((a11, a12,  a1), (a21, a22,  a2), (a31, a32,  a3))
     
    
data Point = Point {x::Double, y::Double}
instance Eq Point where
    (==) a b = x a == x b && y a == y b 
instance Show Point where
    show Point {x=_x, y=_y} = "("++(show _x)++", "++(show _y)++")" 

data LineShape = Ivl { beg::Point, end::Point} 
                |Arc { beg::Point, a::Double, ct::Point, dir::Direction}  
instance Eq LineShape where 
    (==) Ivl { beg=b1, end=e1} Ivl { beg=b2, end=e2} = b1==b2 && e1 == e2
    (==) Arc { beg=b1, a=angle1, ct=c1, dir=d1} Arc { beg=b2, a=angle2, ct=c2, dir=d2} = b1==b2 && angle1 == angle2 && c1==c2 && d1==d2
    (==) _ _ = False


lieson :: (Point, Point) -> Point -> Bool
lieson (Point{x=x1, y=y1},Point{x=x2, y=y2})  Point{x=x3, y=y3} 
    | x1 == x3 = x2 == x3 && (y1==y3 || y2==y3||((y2-y3)/(y1-y3))<0)
    | y1 == y3 = y2 == y3 && (x1==x3 || x2==x3||((x2-x3)/(x1-x3))<0)
    | otherwise = ((x2-x3)/(x1-x3))<0 && ((x2-x3)/(x1-x3))==((y2-y3)/(y1-y3))
    
lineq :: Point -> Point -> (Double, Double, Double)
lineq p1@Point{x=x1, y=y1} p2@Point{x=x2, y=y2} 
    | x1==0 && y1==0 && x2 == 0 = (1, 0, 0)
    | x1==0 && y1==0 && y2 == 0 = (0, 1, 0)
    | x1==0 && y1==0 = (1, -x2/y2, 0)
    | x2==0 && y2==0  = lineq p2 p1
    | l1 == l2 = l1 
    | otherwise = (a, b, 1) 
    where 
        l1 = lineq zero p1
        l2 = lineq zero p2
        zero = Point {x=0, y=0}
        (a,b) = lin2slv ((x1, y1, 1.0), (x2, y2, 1.0))
        
lintersect :: (Double, Double, Double) -> (Double, Double, Double) -> Point
lintersect eq1 eq2 =
    Point {x=sx, y=sy} where
         (sx, sy) = lin2slv (eq1, eq2)
         
         
    
lineSIntersection :: LineShape -> LineShape -> Maybe Point
lineSIntersection  Ivl { beg=b1, end=e1} Ivl { beg=b2, end=e2} 
    | b1 == e1 && b2 == e2 = Nothing
    | b1 == e1 = if lieson (b2, e2) b1  then Just b1 else Nothing
    | b2 == e2 = if lieson (b1, e1) b2 then Just b2 else Nothing
    | otherwise = if lieson (b1, e1) isct && lieson (b2, e2) isct then Just isct else Nothing where
        isct = lintersect (lineq b1 e1) (lineq b2 e2) 
    
    
lineSIntersection  Ivl { beg=b1, end=e1} Arc { beg=b2, a=e2, ct=c2, dir=d2} = Nothing
lineSIntersection  Arc { beg=b1, a=e1, ct=c1, dir=d1} Ivl { beg=b2, end=e2} =  
            lineSIntersection  Ivl { beg=b2, end=e2} Arc { beg=b1, a=e1, ct=c1, dir=d1}
lineSIntersection  Arc { beg=b1, a=e1, ct=c1, dir=d1} Arc { beg=b2, a=e2, ct=c2, dir=d2} = Nothing
            
    
type Shape = [LineShape]

firstPoint :: LineShape -> Point
firstPoint Ivl {beg=a, end=b} = a
firstPoint Arc {beg=a, a=b, ct=c, dir=d} = a

lastPoint :: LineShape -> Point
lastPoint Ivl {beg=a, end=b} = b
lastPoint Arc {beg=Point {x=xr, y=yr}, a=and, ct=Point {x=ctx, y=cty}, dir=CounterClockwise} = Point{ x=ex, y=ey } where 
        ex = ctx+cos(ang)*mvdx - sin(ang)*mvdy
        ey = cty+sin(ang)*mvdx + cos(ang)*mvdy
        mvdx = xr-ctx
        mvdy = yr-cty
        ang = 2*pi*and/360
lastPoint Arc {beg=b, a=and, ct=c, dir=Clockwise} = lastPoint Arc {beg=b, a=(-1.0)*and, ct=c, dir=CounterClockwise}

closeProximity Point{x=x1, y=y1} Point{x=x2, y=y2} = abs(x2-x1)+abs(y2-y1) < 0.000001

closed::Shape -> Bool
closed [] = True
closed x@(f:r) = closeProximity  (firstPoint f)  $ (lastPoint . last) x 

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))

distP:: Point -> Point -> Double
distP Point{x=x1, y=y1} Point{x=x2, y=y2} = dist (x1, y1) (x2, y2)

heron :: Point -> Point -> Point -> Double
heron p1 p2 p3 = sqrt(halfper*(halfper-a)*(halfper-b)*(halfper-c)) where
        halfper  =(a+b+c)/2
        a = distP p1 p2
        b = distP p2 p3
        c = distP p3 p1

area :: Shape -> Maybe Double
area s | closed s = Just (calcArea s)
       | otherwise = Nothing where 
       calcArea [] =  0
       calcArea (Ivl { beg=b1, end=e1} : Ivl { beg=b2, end=e2}: r) = (heron b1 e1 e2) +  calcArea (Ivl {beg=b1, end=e2} : r) 
       calcArea (ar@Arc { beg=b1, a=and, ct=c1, dir=d} : r) | abs(and) > 180 = calcArea (Arc { beg=b1, a=and/2, ct=c1, dir=d} : Arc {beg=(lastPoint Arc { beg=b1, a=and/2, ct=c1, dir=d}), a=and/2, ct=c1, dir=d}:r) 
                                                            | otherwise = chunkArea b1 c1 and + calcArea (Ivl {beg=b1, end=(lastPoint ar)} : r) 
       calcArea (iv@Ivl { beg=b1, end=e1} : ar@Arc { beg=b2, a=and, ct=c, dir=d}: r) = calcArea ((ar : r)++[iv]) 
       calcArea _ = 0 where 
       --heron _ _ _ =  1
       chunkArea _ _ _ =  0
       
       

line_shape_is :: Point -> Point -> Shape -> [Point]

line_shape_is  p1@Point{x=x1, y=y1} p2@Point{x=x2, y=y2} shp = tolist $ map (\y -> lineSIntersection (Ivl ex1 ex2) y) shp where
        tolist  [] = []
        tolist (Nothing:pts) = tolist pts
        tolist ((Just x):pts) = x:(tolist pts)
        ex1 = Point {x= x1-100.0*(x2-x1), y=y1-100.0*(y2-y1)}
        ex2 = Point {x= x1+100.0*(x2-x1), y=y1+100.0*(y2-y1)}
        

cauchy :: Complex Double -> [Complex Double] -> Complex Double
cauchy a b = cauchy'  b where
        cauchy' [z1, z2] =  (1.0/(z2-z1))*((log (z2-a)) - (log(z1-a)))
        cauchy' (l:n:r) = (cauchy'  [l, n]) + cauchy' (n:r)
        cauchy' _ = 0.0 :+ 0.0 
    
--isection :: Shape -> Shape -> Maybe Shape

complexifyPoint Point{x=x1, y=y1} = x1 :+ y1

complexifyShape s = concat $ map (\x -> [complexifyPoint $ firstPoint x, complexifyPoint $ lastPoint x]) s

cauchyShape :: Point -> Shape -> Complex Double       
cauchyShape p s = cauchy (complexifyPoint p) (complexifyShape s) 
    
    
    
data Cell = Omni {center::Point, range::Double} 
            |Sectorized {center::Point, direction::Point, angle::Double, range::Double}
                  
instance Eq Cell where 
    (==)  Omni {center=c1, range=r1}  Omni {center=c2, range=r2} = c1==c2 && r1==r2
    (==)  Sectorized {center=c1, direction=d1, angle=a1, range=r1} Sectorized {center=c2, direction=d2, angle=a2, range=r2} = c1==c2&&a1==a2&&d1==d2&&r1==r2
    (==)  _ _ = False
    
instance Show Cell where 
    show Omni {center=c1, range=r1} = "Omni Cell center="++(show c1)++", range= "++(show r1)
    show Sectorized {center=c1, direction=d1, angle=a1, range=r1} = "Sectorized cell center="++(show c1)++
                                        ", pointing to "++(show d1)++", angle="++(show a1)++", range= "++(show r1)
                                        
type ENB = [Cell]                                        
                     
tabmetrics :: (a -> a -> Double) -> [(a,a)] -> [((a,a),Double)]
tabmetrics f l = map (\x -> (x, (uncurry f) x))  l           

purify :: (Eq a) => [(a,a)] -> [(a,a)]
purify = nubBy (\(x,y) (z,t)-> ((x==z)&&(y==t))||(x==t)&&(y==z)) . filter  (uncurry (/=)) 

preparemetrics :: (Eq a) => (a -> a -> Double) -> [a] -> [((a,a), Double)]
preparemetrics m l = sortBy (comparing (((-1)*).snd)) $ tabmetrics m $ purify [(x,y)|x<-l, y<-l]

clusterize :: (Eq a) => (a -> a -> Double) -> Int -> [a] -> [[a]]
clusterize m limit initial = clusterize' [] structured where
    structured = preparemetrics m initial
    clusterize' :: Eq a => [[a]] -> [((a,a),Double)] -> [[a]]
    clusterize' clusters [] = clusters
    clusterize' clusters (((a,b),mt):r) | isNothing a_cl  && isNothing b_cl  = clusterize' ([a,b]:clusters) r
                                        | isNothing a_cl = if canBeAdded a b_cl then clusterize' (updateCluster a b_cl clusters) r else clusterize' ([a]:clusters) r
                                        | isNothing b_cl = clusterize' clusters (((b,a),mt):r)
                                        | otherwise = if canBeMerged a_cl b_cl then clusterize' ((clusterSum a_cl b_cl):(remove a_cl $ remove b_cl $ clusters)) r else clusterize' clusters r where
        canBeAdded elem Nothing = False
        canBeAdded elem (Just c) = length c < limit
        updateCluster elem Nothing l = l
        updateCluster elem (Just c) l = map (\x -> if(c==x) then (elem:x) else x) l
        canBeMerged (Just c1) (Just c2) = ((length c1) + (length c2)) <= limit
        canBeMerged _ _ = False
        clusterSum (Just c1) (Just c2) = c1++c2
        clusterSum (Just c) Nothing = c
        clusterSum Nothing (Just c) = c
        clusterSum Nothing Nothing  = []
        remove Nothing x = x
        remove (Just c) x = filter (c /= ) x
        a_cl = find (\x -> elem a x) clusters
        b_cl = find (\x -> elem b x) clusters
        
calcobj ::  (Eq a) => (a -> a -> Double)  -> [[a]] -> Double
calcobj m cl = sum $ map partial cl where
    partial s = (sum $ map (uncurry m) [(i,j)|i<-s, j<-s]) / 2
    
f = (\x y -> (map fromInteger [0 ,0 ,1 ,0 ,0 ,2 ,0 ,0 ,0 ,0 ,1 ,3 ,0 ,0 ,1 ,2 ,0 ,3 ,3 ,0 ,0 ,0 ,2 ,0 ,1 ,0 ,1 ,0 ,1 ,0 ,3 ,3 ,0 ,3 ,1 ,0 ,2 ,0 ,3 ,3 ,1 ,2 ,0 ,0 ,3 ,0 ,0 ,0 ,3 ,0 ,2 ,0 ,0 ,3 ,0 ,0 ,1 ,3 ,0 ,2 ,0 ,0 ,0 ,0 ,0 ,3 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,3 ,0 ,0 ,0 ,0 ,3 ,0 ,1 ,3 ,0 ,0 ,1 ,2 ,2 ,3 ,0 ,0 ,0 ,2 ,0 ,1 ,2 ,0 ,2 ,0 ,0 ,2 ,0 ,0 ,3 ,1 ,0 ,3 ,3 ,0 ,0 ,2 ,0 ,1 ,1 ,2 ,0 ,1 ,0 ,3 ,0 ,0 ,1 ,0 ,0 ,2 ,3 ,0 ,3 ,0 ,0 ,0 ,0 ,1 ,3 ,0 ,0 ,0 ,0 ,3 ,0 ,0 ,0 ,0 ,0 ,2 ,3 ,3 ,0 ,0 ,2 ,0 ,0 ,3 ,0 ,1 ,3 ,0 ,0 ,0 ,3 ,0 ,1 ,3 ,0 ,3 ,0 ,0 ,0 ,0 ,0 ,0 ,3 ,0 ,2 ,0 ,0 ,0 ,2 ,2 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,2 ,0 ,0 ,0 ,0 ,0 ,0 ,1 ,3 ,0 ,2 ,0 ,1 ,1 ,3 ,2 ,0 ,0 ,2 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,2 ,0 ,2 ,0 ,3 ,3 ,0 ,0 ,0 ,0 ,0 ,0 ,3 ,0 ,0 ,0 ,0 ,1 ,3 ,0 ,0 ,2 ,3 ,0 ,0 ,0 ,0 ,0 ,2 ,1 ,1 ,3 ,0 ,0 ,0 ,0 ,0 ,0 ,3 ,0 ,0 ,0 ,2 ,0 ,0 ,3 ,3 ,0 ,0 ,1 ,3 ,0 ,2 ,0 ,0 ,1 ,0 ,0 ,2 ,0 ,1 ,0 ,3 ,0 ,1 ,1 ,0 ,3 ,1 ,2 ,0 ,1 ,0 ,1 ,0 ,3 ,3 ,2 ,0 ,1 ,0 ,0 ,0 ,0 ,2 ,0 ,0 ,0 ,2 ,0 ,0 ,3 ,0 ,3 ,0 ,0 ,0 ,0 ,1 ,0 ,0 ,2 ,0 ,2 ,0 ,2 ,1 ,0 ,0 ,1 ,0 ,0 ,0 ,0 ,2 ,0 ,0 ,1 ,0 ,0 ,0 ,1 ,2 ,0 ,3 ,0 ,3 ,0 ,2 ,0 ,0 ,0 ,2 ,2 ,0 ,2 ,0 ,0 ,0 ,2 ,1 ,0 ,1 ,2 ,3 ,3 ,0 ,0 ,0 ,3 ,3 ,0 ,2 ,0 ,2 ,3 ,2 ,3 ,0 ,0 ,2 ,1 ,0 ,0 ,0 ,3 ,2 ,3 ,0 ,0 ,0 ,3 ,0 ,1 ,0 ,0 ,0 ,0 ,0 ,2 ,0 ,2 ,0 ,0])!!(x*20+y))     
c =  clusterize f  3 [0..19]
o =  calcobj f c       
                     
                     
                     
                     
                     
                     
                     

                                        