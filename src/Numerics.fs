module Numerics

// ----------------------------------------------------------------
// Misc math and numerical types
[<AutoOpen>]
module Vectors =
    [<Struct>]
    type Vector = {
        x : int
        y : int
    }

    module Vector =
        let init x y = { x = x; y = y }
        let zero = init 0 0
        let left = init -1 0
        let right = init 1 0
        let up = init 0 -1
        let down = init 0 1

    /// Note by declaring within same module, these are effectively part
    /// of the type. We declare them separately in order to use Vector
    /// module functions.
    type Vector with
        static member inline (~-) v = Vector.init -v.x -v.y
        static member inline (+) (a, b) = Vector.init (a.x + b.x) (a.y + b.y)
        static member inline (-) (a, b) = Vector.init (a.x - b.x) (a.y - b.y)
        static member inline (*) (a, c) = Vector.init (a.x * c) (a.y * c)
        static member inline (*) (a, b) = Vector.init (a.x * b.x) (a.y * b.y)
        static member inline (/) (a, c) = Vector.init (a.x / c) (a.y / c)
        static member inline (/) (a, b) = Vector.init (a.x / b.x) (a.y / b.y)

[<AutoOpen>]
module Colors =
    [<Struct>]
    type Rgba =
        val r : float32
        val g : float32
        val b : float32
        val a : float32
        new(r, g, b, a) = { r = r; g = g; b = b; a = a; }
        static member inline (+) (a: Rgba, b: Rgba) = Rgba(a.r + b.r, a.g + b.g, a.b + b.b, a.a + b.a)
        static member inline (-) (a: Rgba, b: Rgba) = Rgba(a.r - b.r, a.g - b.g, a.b - b.b, a.a - b.a)
        static member inline (*) (a: Rgba, b: Rgba) = Rgba(a.r * b.r, a.g * b.g, a.b * b.b, a.a * b.a)
        static member inline (*) (a: Rgba, c) = Rgba(a.r * c, a.g * c, a.b * c, a.a * c)
        member inline c.multiplyAlpha a = Rgba(c.r, c.g, c.b, c.a * a)

    module Rgba =
        let inline init r g b a = Rgba(r, g, b, a)
        let inline all x = init x x x x
        let inline rgb r g b = init r g b 1.0f
        let inline alpha a = init 1.0f 1.0f 1.0f a
        let inline toRgb (c : Rgba) = init c.r c.g c.b 1.0f
        let inline multiplyRgb x (c : Rgba) = init (c.r * x) (c.g * x) (c.b * x) c.a
        let inline multiplyAlpha a (c : Rgba) = c.multiplyAlpha a
        let luminance x = rgb x x x
        let zero = all 0.0f
        let one = all 1.0f
        
        // let lerp (min: Rgba) (max: Rgba) t =
        //     Rgba(  lerp min.r max.r t,
        //         lerp min.g max.g t,
        //         lerp min.b max.b t,
        //         lerp min.a max.a t)

    type Hsva =
        val h : float32
        val s : float32
        val v : float32
        val a : float32
        new(h, s, v, a) = { h = h; s = s; v = v; a = a; }

    module Hsva =
        let init h s v a = Hsva(h, s, v, a)

        let shiftHue shift (c : Hsva) =
            Hsva(c.h + shift, c.s, c.v, c.a)

        let private frac (x : float32) = x - (floor x)

        let fromRgba (c : Rgba) =
            let m = min (min c.r c.g) c.b
            let v = max (max c.r c.g) c.b
            let s = if v > System.Single.Epsilon then 1.0f - m / v else 0.0f
            let l = (m + v) / 2.0f
            if l < System.Single.Epsilon || v < m 
            then Hsva(0.0f, s, v, c.a)
            else
                let vm = v - m
                let r2 = (v - c.r) / vm
                let g2 = (v - c.g) / vm
                let b2 = (v - c.b) / vm
                let hx = 
                    if c.r = v then if c.g = m then 5.0f + b2 else 1.0f - g2
                    else if c.g = v then if c.b = m then 1.0f + r2 else 3.0f - b2
                    else if c.r = m then 3.0f + g2 else 5.0f - r2
                    / 6.0f
                let h = if hx >= 1.0f then hx - 1.0f else hx
                Hsva(h, s, v, c.a)

        let toRgba (c : Hsva) =
            // from: http://www.alvyray.com/Papers/hsv2rgb.htm
            // H is given on [0, 6] or UNDEFINED. S and V are given on [0, 1].  
            // RGB are each returned on [0, 1].
            let h = (frac c.h) * 6.0f
            let v = c.v
            let i = int32(floor h)
            let f = frac h
            // if i is even
            let g = if (i &&& 1) = 0 then 1.0f - f else f
            let m = v * (1.0f - c.s)
            let n = v * (1.0f - c.s * g)
            match i with
            | 6 -> Rgba(v, n, m, c.a)
            | 0 -> Rgba(v, n, m, c.a)
            | 1 -> Rgba(n, v, m, c.a)
            | 2 -> Rgba(m, v, n, c.a)
            | 3 -> Rgba(m, n, v, c.a)
            | 4 -> Rgba(n, m, v, c.a)
            | _ -> Rgba(v, m, n, c.a)

module Noise =
    let grad3 =
        array2D
            [[1.0f;1.0f;0.0f];[-1.0f;1.0f;0.0f];[1.0f;-1.0f;0.0f];[-1.0f;-1.0f;0.0f];
            [1.0f;0.0f;1.0f];[-1.0f;0.0f;1.0f];[1.0f;0.0f;-1.0f];[-1.0f;0.0f;-1.0f];
            [0.0f;1.0f;1.0f];[0.0f;-1.0f;1.0f];[0.0f;1.0f;-1.0f];[0.0f;-1.0f;-1.0f]];

    // Permutation table. This is just a random jumble of all numbers 0-255;
    // repeated twice to avoid wrapping the index at 255 for each lookup.
    let perm =
        [| 151;160;137;91;90;15;
        131;13;201;95;96;53;194;233;7;225;140;36;103;30;69;142;8;99;37;240;21;10;23;
        190; 6;148;247;120;234;75;0;26;197;62;94;252;219;203;117;35;11;32;57;177;33;
        88;237;149;56;87;174;20;125;136;171;168; 68;175;74;165;71;134;139;48;27;166;
        77;146;158;231;83;111;229;122;60;211;133;230;220;105;92;41;55;46;245;40;244;
        102;143;54; 65;25;63;161; 1;216;80;73;209;76;132;187;208; 89;18;169;200;196;
        135;130;116;188;159;86;164;100;109;198;173;186; 3;64;52;217;226;250;124;123;
        5;202;38;147;118;126;255;82;85;212;207;206;59;227;47;16;58;17;182;189;28;42;
        223;183;170;213;119;248;152; 2;44;154;163; 70;221;153;101;155;167; 43;172;9;
        129;22;39;253; 19;98;108;110;79;113;224;232;178;185; 112;104;218;246;97;228;
        251;34;242;193;238;210;144;12;191;179;162;241; 81;51;145;235;249;14;239;107;
        49;192;214; 31;181;199;106;157;184; 84;204;176;115;121;50;45;127; 4;150;254;
        138;236;205;93;222;114;67;29;24;72;243;141;128;195;78;66;215;61;156;180;
        151;160;137;91;90;15; |]
        |> Seq.replicate 2
        |> Seq.collect id
        |> Seq.toArray

    let Sqrt3 = 1.7320508075688772935274463415059f
    let F2 = 0.5f * (Sqrt3 - 1.0f);
    let G2 = (3.0f - Sqrt3) / 6.0f;

    let inline dot2 (g : float32[,]) gi x y = g.[gi, 0] * x + g.[gi, 1] * y

    let sample2 x y =
        let s = (x + y) * F2
        let realI = x + s
        let realJ = y + s
        let i = int(if realI > 0.0f then realI else realI - 1.0f)
        let j = int(if realJ > 0.0f then realJ else realJ - 1.0f)    
        let t = float32(i + j) * G2
        let X0 = float32(i) - t
        let Y0 = float32(j) - t
        let x0 = x - X0
        let y0 = y - Y0
        let i1 = if x0 > y0 then 1 else 0
        let j1 = 1 - i1
        let x1 = x0 - float32(i1) + G2
        let y1 = y0 - float32(j1) + G2
        let x2 = x0 - 1.0f + 2.0f * G2
        let y2 = y0 - 1.0f + 2.0f * G2
        let ii = i &&& 255
        let jj = j &&& 255
        let gi0 = perm.[ii + perm.[jj]] % 12
        let gi1 = perm.[ii + i1 + perm.[jj + j1]] % 12
        let gi2 = perm.[ii + 1 + perm.[jj + 1]] % 12
        let t0 = 0.5f - x0 * x0 - y0 * y0
        let n0 =
            if (t0 < 0.0f) then 0.0f
            else
                let t02 = t0 * t0
                t02 * t02 * (dot2 grad3 gi0 x0 y0)
        let t1 = 0.5f - x1 * x1 - y1 * y1
        let n1 =
            if (t1 < 0.0f) then 0.0f
            else
                let t12 = t1 * t1
                t12 * t12 * (dot2 grad3 gi1 x1 y1)
        let t2 = 0.5f - x2 * x2 - y2 * y2
        let n2 =
            if (t2 < 0.0f) then 0.0f
            else
                let t22 = t2 * t2
                t22 * t22 * (dot2 grad3 gi2 x2 y2)
        70.0f * (n0 + n1 + n2)
