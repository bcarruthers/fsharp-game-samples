module Numerics

[<Struct>]
type Interval = {
    min : int
    max : int
}

[<Struct>]
type Vec2 = {
    x : int
    y : int
} with
    static member inline (+) (a: Vec2, b: Vec2) = { x = a.x + b.x; y = a.y + b.y }
    static member inline (-) (a: Vec2, b: Vec2) = { x = a.x - b.x; y = a.y - b.y }
    static member inline (*) (a: Vec2, c) = { x = a.x * c; y = a.y * c }

[<Struct>]
type Rect = {
    ix : Interval
    iy : Interval
}

[<Struct>]
type Intervalf = {
    min : float32
    max : float32
}

[<Struct>]
type Vec2f = {
    x : float32
    y : float32
}

[<Struct>]
type Rectf = {
    ix : Intervalf
    iy : Intervalf
}

type Interval with
    member c.len = c.max - c.min

module Interval =
    let length c = c.max - c.min
    let init min max = { Interval.min = min; max = max }
    let zero = init 0 0
    let sized min size = init min (min + size)

module Intervalf =
    let length c = c.max - c.min
    let init min max = { Intervalf.min = min; max = max }
    let zero = init 0.0f 0.0f

module Vec2 =
    let init x y = { Vec2.x = x; y = y }
    let zero = init 0 0
    let one = init 1 1

module Vec2f =
    let init x y = { Vec2f.x = x; y = y }
    let fromInt (v: Vec2) = init (float32 v.x) (float32 v.y)

type Rect with
    member c.min = Vec2.init c.ix.min c.iy.min
    member c.max = Vec2.init c.ix.max c.iy.max

module Rect =
    let init ix iy = { Rect.ix = ix; iy = iy }
    let zero = init Interval.zero Interval.zero

    let bounds (min : Vec2) (max : Vec2) = 
        init (Interval.init min.x max.x) (Interval.init min.y max.y)
            
    let sized (min : Vec2) (size : Vec2) = bounds min (min + size)

module Rectf =
    let init ix iy = { Rectf.ix = ix; iy = iy }
    let zero = init Intervalf.zero Intervalf.zero

    let bounds (min : Vec2f) (max : Vec2f) = 
        init (Intervalf.init min.x max.x) (Intervalf.init min.y max.y)
