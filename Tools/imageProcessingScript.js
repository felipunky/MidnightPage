"use strict";

var vertexShaderSource = `#version 300 es

// an attribute is an input (in) to a vertex shader.
// It will receive data from a buffer
in vec2 a_position;
in vec2 a_texCoord;

// Used to pass in the resolution of the canvas
uniform vec2 u_resolution;

// Used to pass the texture coordinates to the fragment shader
out vec2 v_texCoord;

// all shaders have a main function
void main() {

  // convert the position from pixels to 0.0 to 1.0
  vec2 zeroToOne = a_position / u_resolution;

  // convert from 0->1 to 0->2
  vec2 zeroToTwo = zeroToOne * 2.0;

  // convert from 0->2 to -1->+1 (clipspace)
  vec2 clipSpace = zeroToTwo - 1.0;

  gl_Position = vec4(clipSpace * vec2(1, -1), 0, 1);

  // pass the texCoord to the fragment shader
  // The GPU will interpolate this value between points.
  v_texCoord = a_texCoord;
}
`;

var fragmentShaderSource = `#version 300 es

// fragment shaders don't have a default precision so we need
// to pick one. highp is a good default. It means "high precision"
precision highp float;

// our texture
uniform sampler2D u_image0;
uniform sampler2D u_image1;

// the texCoords passed in from the vertex shader.
in vec2 v_texCoord;

// Used to pass in the resolution of the canvas
uniform vec2 u_resolution;
uniform vec2 u_noise;
uniform vec3 u_Move;
uniform float u_softness;
uniform int u_Shape;
uniform float u_Size;
uniform vec2 u_BrightnessContrast;
uniform vec3 u_HSV;
uniform float u_WidthSDF;
uniform int u_Vignette;
uniform int u_Rings;
uniform int u_ScaleLogo;
uniform float u_NoiseTime;
uniform float u_InnerWidth;

// we need to declare an output for the fragment shader
out vec4 outColor;

float dot2( in vec2 v ) { return dot(v,v); }

// signed distance to a n-star polygon with external angle en
float sdStar(in vec2 p, in float r, in int n, in float m) // m=[2,n]
{
    // these 4 lines can be precomputed for a given shape
    float an = 3.141593/float(n);
    float en = 3.141593/m;
    vec2  acs = vec2(cos(an),sin(an));
    vec2  ecs = vec2(cos(en),sin(en)); // ecs=vec2(0,1) and simplify, for regular polygon,

    // symmetry (optional)
    p.x = abs(p.x);
    
    // reduce to first sector
    float bn = mod(atan(p.x,p.y),2.0*an) - an;
    p = length(p)*vec2(cos(bn),abs(sin(bn)));

    // line sdf
    p -= r*acs;
    p += ecs*clamp( -dot(p,ecs), 0.0, r*acs.y/ecs.y);
    return length(p)*sign(p.x);
}

mat2 rot(const in float a)
{
    vec2 sinCos = vec2(sin(a), cos(a));
    return mat2(sinCos.y, -sinCos.x,
                sinCos.x,  sinCos.y);
}

float sdRoundedBox( in vec2 p, in vec2 b, in vec4 r )
{
    r.xy = (p.x>0.0)?r.xy : r.zw;
    r.x  = (p.y>0.0)?r.x  : r.y;
    vec2 q = abs(p)-b+r.x;
    return min(max(q.x,q.y),0.0) + length(max(q,0.0)) - r.x;
}

float sdCircle( vec2 p, float r )
{
    return length(p) - r;
}

float sdEquilateralTriangle( in vec2 p, in float r )
{
    const float k = sqrt(3.0);
    p.x = abs(p.x) - r;
    p.y = p.y + r/k;
    if( p.x+k*p.y>0.0 ) p = vec2(p.x-k*p.y,-k*p.x-p.y)/2.0;
    p.x -= clamp( p.x, -2.0*r, 0.0 );
    return -length(p)*sign(p.y);
}

float sdPentagon( in vec2 p, in float r )
{
    const vec3 k = vec3(0.809016994,0.587785252,0.726542528);
    p.x = abs(p.x);
    p -= 2.0*min(dot(vec2(-k.x,k.y),p),0.0)*vec2(-k.x,k.y);
    p -= 2.0*min(dot(vec2( k.x,k.y),p),0.0)*vec2( k.x,k.y);
    p -= vec2(clamp(p.x,-r*k.z,r*k.z),r);    
    return length(p)*sign(p.y);
}

float sdHexagon( in vec2 p, in float r )
{
    const vec3 k = vec3(-0.866025404,0.5,0.577350269);
    p = abs(p);
    p -= 2.0*min(dot(k.xy,p),0.0)*k.xy;
    p -= vec2(clamp(p.x, -k.z*r, k.z*r), r);
    return length(p)*sign(p.y);
}

float sdStar5(in vec2 p, in float r, in float rf)
{
    const vec2 k1 = vec2(0.809016994375, -0.587785252292);
    const vec2 k2 = vec2(-k1.x,k1.y);
    p.x = abs(p.x);
    p -= 2.0*max(dot(k1,p),0.0)*k1;
    p -= 2.0*max(dot(k2,p),0.0)*k2;
    p.x = abs(p.x);
    p.y -= r;
    vec2 ba = rf*vec2(-k1.y,k1.x) - vec2(0,1);
    float h = clamp( dot(p,ba)/dot(ba,ba), 0.0, r );
    return length(p-ba*h) * sign(p.y*ba.x-p.x*ba.y);
}

float sdHeart( in vec2 p )
{
    p.x = abs(p.x);

    if( p.y+p.x>1.0 )
        return sqrt(dot2(p-vec2(0.25,0.75))) - sqrt(2.0)/4.0;
    return sqrt(min(dot2(p-vec2(0.00,1.00)),
                    dot2(p-0.5*max(p.x+p.y,0.0)))) * sign(p.x-p.y);
}

float sdMoon(vec2 p, float d, float ra, float rb )
{
    p.y = abs(p.y);
    float a = (ra*ra - rb*rb + d*d)/(2.0*d);
    float b = sqrt(max(ra*ra-a*a,0.0));
    if( d*(p.x*b-p.y*a) > d*d*max(b-p.y,0.0) )
          return length(p-vec2(a,b));
    return max( (length(p          )-ra),
               -(length(p-vec2(d,0))-rb));
}

float sdCross( in vec2 p, in vec2 b, float r ) 
{
    p = abs(p); p = (p.y>p.x) ? p.yx : p.xy;
    vec2  q = p - b;
    float k = max(q.y,q.x);
    vec2  w = (k>0.0) ? q : vec2(b.y-p.x,-k);
    return sign(k)*length(max(w,0.0)) + r;
}

// https://www.shadertoy.com/view/3dKSDc
float sdRoundedX( in vec2 p, in float w, in float r )
{
    p = abs(p);
    return length(p-min(p.x+p.y,w)*0.5) - r;
}

// https://www.shadertoy.com/view/MlycD3
float sdTrapezoid( in vec2 p, in vec2 a, in vec2 b, in float ra, float rb )
{
    float rba  = rb-ra;
    float baba = dot(b-a,b-a);
    float papa = dot(p-a,p-a);
    float paba = dot(p-a,b-a)/baba;
    float x = sqrt( papa - paba*paba*baba );
    float cax = max(0.0,x-((paba<0.5)?ra:rb));
    float cay = abs(paba-0.5)-0.5;
    float k = rba*rba + baba;
    float f = clamp( (rba*(x-ra)+paba*baba)/k, 0.0, 1.0 );
    float cbx = x-ra - f*rba;
    float cby = paba - f;
    float s = (cbx < 0.0 && cay < 0.0) ? -1.0 : 1.0;
    return s*sqrt( min(cax*cax + cay*cay*baba,
                       cbx*cbx + cby*cby*baba) );
}

// https://www.shadertoy.com/view/slj3Dd
float sdArrow( in vec2 p, vec2 a, vec2 b, float w1, float w2 )
{
    // constant setup
    const float k = 3.0;   // arrow head ratio
	vec2  ba = b - a;
    float l2 = dot(ba,ba);
    float l = sqrt(l2);

    // pixel setup
    p = p-a;
    p = mat2(ba.x,-ba.y,ba.y,ba.x)*p/l;
    p.y = abs(p.y);
    vec2 pz = p-vec2(l-w2*k,w2);

    // === distance (four segments) === 

    vec2 q = p;
    q.x -= clamp( q.x, 0.0, l-w2*k );
    q.y -= w1;
    float di = dot(q,q);
    //----
    q = pz;
    q.y -= clamp( q.y, w1-w2, 0.0 );
    di = min( di, dot(q,q) );
    //----
    if( p.x<w1 ) // conditional is optional
    {
    q = p;
    q.y -= clamp( q.y, 0.0, w1 );
    di = min( di, dot(q,q) );
    }
    //----
    if( pz.x>0.0 ) // conditional is optional
    {
    q = pz;
    q -= vec2(k,-1.0)*clamp( (q.x*k-q.y)/(k*k+1.0), 0.0, w2 );
    di = min( di, dot(q,q) );
    }
    
    // === sign === 
    
    float si = 1.0;
    float z = l - p.x;
    if( min(p.x,z)>0.0 ) //if( p.x>0.0 && z>0.0 )
    {
      float h = (pz.x<0.0) ? w1 : z/k;
      if( p.y<h ) si = -1.0;
    }
    return si*sqrt(di);
}

float sdEllipse( in vec2 p, in vec2 ab )
{
    p = abs(p); if( p.x > p.y ) {p=p.yx;ab=ab.yx;}
    float l = ab.y*ab.y - ab.x*ab.x;
    float m = ab.x*p.x/l;      float m2 = m*m; 
    float n = ab.y*p.y/l;      float n2 = n*n; 
    float c = (m2+n2-1.0)/3.0; float c3 = c*c*c;
    float q = c3 + m2*n2*2.0;
    float d = c3 + m2*n2;
    float g = m + m*n2;
    float co;
    if( d<0.0 )
    {
        float h = acos(q/c3)/3.0;
        float s = cos(h);
        float t = sin(h)*sqrt(3.0);
        float rx = sqrt( -c*(s + t + 2.0) + m2 );
        float ry = sqrt( -c*(s - t + 2.0) + m2 );
        co = (ry+sign(l)*rx+abs(g)/(rx*ry)- m)/2.0;
    }
    else
    {
        float h = 2.0*m*n*sqrt( d );
        float s = sign(q+h)*pow(abs(q+h), 1.0/3.0);
        float u = sign(q-h)*pow(abs(q-h), 1.0/3.0);
        float rx = -s - u - c*4.0 + 2.0*m2;
        float ry = (s - u)*sqrt(3.0);
        float rm = sqrt( rx*rx + ry*ry );
        co = (ry/sqrt(rm-rx)+2.0*g/rm-m)/2.0;
    }
    vec2 r = ab * vec2(co, sqrt(1.0-co*co));
    return length(r-p) * sign(p.y-r.y);
}

// https://www.shadertoy.com/view/ltBSWd
vec3 HSVtoRGB( in vec3 c )
{
    vec3 rgb = clamp( abs(mod(c.x*6.0+vec3(0.0,4.0,2.0),6.0)-3.0)-1.0, 0.0, 1.0 );
    return c.z * mix( vec3(1.0), rgb, c.y);
}

vec3 RGBtoHSV(vec3 rgb)
{
    // Hue: red = 0/6, yellow = 1/6, green = 2/6,
    //      cyan = 3/6, blue = 4/6, magenta = 5/6
    vec3 hsv;
    float cmax = max(rgb.r, max(rgb.g, rgb.b));
    float cmin = min(rgb.r, min(rgb.g, rgb.b));
    
    hsv.z = cmax; // value

    float chroma = cmax - cmin;
    {
        hsv.y = chroma / cmax; // saturation

        //if(cmax == rgb.r)
        if(rgb.r > rgb.g && rgb.r > rgb.b)
        {
            hsv.x = (0.0 + (rgb.g - rgb.b) / chroma) / 6.0; // hue
        }
        //else if(cmax == rgb.m_Green)
        else if(rgb.g > rgb.b)
        {
            hsv.x = (2.0 + (rgb.b - rgb.r) / chroma) / 6.0; // hue
        }
        else
        {
            hsv.x = (4.0 + (rgb.r - rgb.g) / chroma) / 6.0; // hue
        }

        // Make sure hue is in range [0..1]
        hsv.x = fract(hsv.x);
    }
    return hsv;
}

// Simplex 2D noise
//
vec3 permute(vec3 x) { return mod(((x*34.0)+1.0)*x, 289.0); }

float snoise(vec2 v)
{
    const vec4 C = vec4(0.211324865405187, 0.366025403784439,
            -0.577350269189626, 0.024390243902439);
    vec2 i  = floor(v + dot(v, C.yy) );
    vec2 x0 = v -   i + dot(i, C.xx);
    vec2 i1;
    i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
    vec4 x12 = x0.xyxy + C.xxzz;
    x12.xy -= i1;
    i = mod(i, 289.0);
    vec3 p = permute( permute( i.y + vec3(0.0, i1.y, 1.0 ))
    + i.x + vec3(0.0, i1.x, 1.0 ));
    vec3 m = max(0.5 - vec3(dot(x0,x0), dot(x12.xy,x12.xy),
    dot(x12.zw,x12.zw)), 0.0);
    m = m*m ;
    m = m*m ;
    vec3 x = 2.0 * fract(p * C.www) - 1.0;
    vec3 h = abs(x) - 0.5;
    vec3 ox = floor(x + 0.5);
    vec3 a0 = x - ox;
    m *= 1.79284291400159 - 0.85373472095314 * ( a0*a0 + h*h );
    vec3 g;
    g.x  = a0.x  * x0.x  + h.x  * x0.y;
    g.yz = a0.yz * x12.xz + h.yz * x12.yw;
    return 130.0 * dot(m, g);
}

// GIMP Source
#define G_PI_4 0.78539816339744830962
vec3 brightnessContrast( const in vec3 colIn, const in float brightness, const in float contrast )
{
  float slant = tan ((contrast + 1.0) * G_PI_4);
  vec3 value = colIn;
  if (brightness < 0.0)
  {
    value = value * (1.0 + brightness);
  }
  else
  {
    value = value + ((1.0 - value) * brightness);
  }
  return (value - 0.5) * slant + 0.5;
}

void main() 
{
    vec2 r = vec2(1.0);
    if (u_resolution.x < u_resolution.y)
    {
      r.x = u_resolution.x / u_resolution.y;
    }
    else
    {
      r.y = u_resolution.y / u_resolution.x;
    }
    vec2 p = v_texCoord*2.-1.;
    vec2 pLogo = p;
    p *= r;
    float n = snoise(p * u_noise.y + u_NoiseTime);
    p += n * u_noise.x;
    vec2 p0 = p;
    p += u_Move.xy * vec2(-1, 1);
    
    p *= rot(radians(u_Move.z));
    
    float d0 = sdRoundedBox( p0, vec2( .85 ), vec4( 0. ) );
    float d = 0.0;
    if (u_Shape == 0)
    {
      d = sdStar( p, u_Size, 5, 3.);
    }
    else if (u_Shape == 1)
    {
      d = sdRoundedBox( p, vec2( u_Size*.5 ), vec4( 0. ) );
    }
    else if (u_Shape == 2)
    {
      d = sdCircle( p, u_Size * 0.6 );
    }
    else if (u_Shape == 3)
    {
      d = sdEquilateralTriangle( p, u_Size * 0.7 );
    }
    else if (u_Shape == 4)
    {
      d = sdPentagon( p, u_Size * 0.5 );
    }
    else if (u_Shape == 5)
    {
      d = sdHexagon( p, u_Size * 0.5 );
    }
    else if (u_Shape == 6)
    {
      d = sdHeart( vec2( p.x, 1.-p.y-.5 ) * u_Size );
    }
    else if (u_Shape == 7)
    {
      float ra = u_Size;
      float rb = 0.8;
      float di = -0.6;
      d = sdMoon( p, di, ra, rb );
    }
    else if (u_Shape == 8)
    {
      // Size
	    vec2 si = vec2(u_Size, u_Size * 0.5);

      // Corner radious
      float ra = u_InnerWidth;
      d = sdCross( p, si, ra );
    }
    else if (u_Shape == 9)
    {
      float ra = 0.1;
      float rb = 0.8;
      vec2  pa = vec2(0.0, -u_Size);
      vec2  pb = vec2(0.0,  u_Size);
      vec2  pc = vec2(u_Size * 2.0, 0.0);

      // axis aligned trapezoid
      d = sdTrapezoid( p, pa, pb, ra, rb );
    }
    else if (u_Shape == 10)
    {
      // width
	    float wi = u_Size;
      // radious
      float ra = u_InnerWidth;

	    d = sdRoundedX( p, wi, ra );
    }
    else if (u_Shape == 11)
    {
      vec2 a = vec2(-u_Size,0.0);
      vec2 b = vec2( u_Size,0.0);
      float w1 = u_InnerWidth;
      float w2 = w1 + 0.15;
      float th = 0.01;
      // distance
      d = sdArrow(p, a, b, w1, w2) - th;
    }
    else if (u_Shape == 12)
    {
      vec2 ra = vec2(u_Size, u_InnerWidth);
 	    d = sdEllipse( p, ra );
    }
    if (u_Rings > 0)
    {
      float scale = float(u_Rings);
      float widthRings = u_WidthSDF;
      for (int i = 0; i < u_Rings; ++i)
      {
        d = abs(d + widthRings*0.5) - widthRings;
        widthRings /= scale;
      }
    }

    float s = smoothstep(0., u_softness, d);
    
    vec4 tex = texture(u_image0, v_texCoord);
    vec3 hsv = RGBtoHSV(tex.rgb) * ((u_HSV+1.)*2.-1.);
    vec3 rgb = HSVtoRGB(hsv);
    // Brightness/Contrast
    vec3 col = brightnessContrast(rgb.rgb, u_BrightnessContrast.x, u_BrightnessContrast.y);
    col = mix(vec3(0), col, (u_Rings == 0 ? 1.-s : s));
    if (u_Vignette == 1)
    {
      float s0 = smoothstep(0., u_softness, d0);
      col = mix(col, vec3(0), s0);
    }
    // Logo
    {
      float sca = 7.0-float(u_ScaleLogo);
      float modSca = mod(sca, 2.);
      float scaH = floor(sca*0.5);
      vec3 logoCol = texture(u_image1, fract(v_texCoord*sca)).rgb;
      pLogo = pLogo*(sca/2.);
      pLogo += (modSca != 0. ? .5 : 0.);
      vec2 idLogo = floor(pLogo);
      
      pLogo = fract(pLogo)-.5;
      float dLogo = sdRoundedBox( pLogo + snoise(pLogo * u_noise.y + u_NoiseTime)*u_noise.x, vec2( .4 ), vec4( 0. ) );
      float sLogo = smoothstep(0., u_softness, dLogo);
      if (idLogo.x == -scaH && idLogo.y == sca-1.-scaH)
      {
        col = mix(logoCol, col, sLogo);
      }
    }
    outColor = vec4(col, 1.0);
}
`;

function imageIsLoaded(image) 
{
  return new Promise(resolve => 
  {
    image.onload = () => resolve()
    image.onerror = () => resolve()
  })
}

const imgNames = ["IMG_1524.jpg", "midnightLogo.jpg"];
const images = [];
for (var i = 0; i < imgNames.length; ++i)
{
  images.push(new Image());
  images[i].src = "../Images/" + imgNames[i];
}

Promise.all(images.map(imageIsLoaded)).then(() => 
{
  main();
})

let gui = new dat.GUI();

function main()
{
  gui.destroy();
  gui = new dat.GUI();
  // Get A WebGL context
  /** @type {HTMLCanvasElement} */
  var canvas = document.querySelector("#canvas");
  canvas.width = images[0].width;
  canvas.height = images[0].height;
  var gl = canvas.getContext("webgl2");
  if (!gl) 
  {
    return;
  }

  // setup GLSL program
  var program = webglUtils.createProgramFromSources(gl,
      [vertexShaderSource, fragmentShaderSource]);

  // look up where the vertex data needs to go.
  var positionAttributeLocation = gl.getAttribLocation(program, "a_position");
  var texCoordAttributeLocation = gl.getAttribLocation(program, "a_texCoord");

  // lookup uniforms
  var resolutionLocation = gl.getUniformLocation(program, "u_resolution");
  var imageLocations = [];
  for (var i = 0; i < images.length; ++i)
  {
    imageLocations.push(gl.getUniformLocation(program, "u_image" + i));
  }
  var noiseLocation = gl.getUniformLocation(program, "u_noise");
  var moveLocation = gl.getUniformLocation(program, "u_Move");
  var softnessLocation = gl.getUniformLocation(program, "u_softness");
  var shapeLocation = gl.getUniformLocation(program, "u_Shape");
  var sizeLocation = gl.getUniformLocation(program, "u_Size");
  var brightnessContrastLocation = gl.getUniformLocation(program, "u_BrightnessContrast");
  var hsvLocation = gl.getUniformLocation(program, "u_HSV");
  var widthSDFLocation = gl.getUniformLocation(program, "u_WidthSDF");
  var scaleLogoLocation = gl.getUniformLocation(program, "u_ScaleLogo");
  var vignetteLocation = gl.getUniformLocation(program, "u_Vignette");
  var ringsLocation = gl.getUniformLocation(program, "u_Rings");
  var noiseTimeLocation = gl.getUniformLocation(program, "u_NoiseTime");
  var innerWidthLocation = gl.getUniformLocation(program, "u_InnerWidth");

  const uniforms = 
  {
    shape: 1,
    size: 1.4,
    innerWidth: 0.5,
    canvasMultiplier: true,
    angle: 0.0,
    noiseStrength: 0.01,
    noiseFrequency: 10.0,
    noiseTime: 0.0,
    rings: 0,
    widthSDF: 0.026,
    X: 0.0,
    Y: 0.0,
    softness: 0.1,
    brightness: -0.087,
    contrast: 0.068,
    hue: 0.0,
    saturation: 0.212,
    value: 0.122,
    vignette: 0,
    vignetteDummy: true,
    miniatureDummy: true,
    logoScale: 4
  }
  let step = 0.001;
  gui.add(uniforms, 'shape', { Star: 0, Box: 1, Circle: 2, Triangle: 3, 
                               Pentagon: 4, Hexagon: 5, Heart: 6,
                               Moon: 7, Cross: 8, Trapezoid: 9,
                               RoundedX: 10, Arrow: 11, Ellipse: 12 } ).name("Shape");
  gui.add(uniforms, 'size', 0.1, 2.0, 0.01).name("Size");
  gui.add(uniforms, 'innerWidth', 0.1, 1.0, 0.01).name("Inner Width");
  gui.add(uniforms, "softness", 0.01, 0.5, step ).name("Softness");
  gui.add(uniforms, 'rings', 0, 4).name("Rings");
  gui.add(uniforms, 'widthSDF', 0.0, 0.1, 0.001).name("Width SDF");
  gui.add(uniforms, "logoScale", 1, 6).name("Scale Logo");
  gui.add(uniforms, 'canvasMultiplier').name("Scale").listen().onChange( function() 
  {
    scaleCheckbox(uniforms.canvasMultiplier);
  });
  gui.add(uniforms, 'vignetteDummy').name("Vignette").listen().onChange( function() 
  {
    vignetteCheckbox();
  });
  gui.add(uniforms, 'miniatureDummy').name("Miniature Download").listen().onChange( function() 
  {
    miniatureCheckbox(uniforms.miniatureDummy);
  });
  var noiseFolder = gui.addFolder("Noise");
  noiseFolder.add( uniforms, "noiseStrength", 0.0, 0.1, 0.01 ).name("Noise Strength");
  noiseFolder.add( uniforms, "noiseFrequency", 0.0, 10.0, 0.01 ).name("Noise Frequency");
  noiseFolder.add( uniforms, "noiseTime", 0.0, 1.0, 0.1 ).name("Noise Time");
  var translateFolder = gui.addFolder("Transform");
  translateFolder.add( uniforms, "angle", 0.0, 360.0, 0.1 ).name("Angle");
  translateFolder.add( uniforms, "X", -1.0, 1.0, 0.01 ).name("Move X");
  translateFolder.add( uniforms, "Y", -1.0, 1.0, 0.01 ).name("Move Y");
  
  var renderFolder = gui.addFolder("Render");
  renderFolder.add( uniforms, "brightness", -1.0, 1.0, step ).name("Brightness");
  renderFolder.add( uniforms, "contrast", -1.0, 1.0, step ).name("Contrast");
  var hsvFolder = renderFolder.addFolder("HSV");
  hsvFolder.add( uniforms, "hue", -1.0, 1.0, step ).name("Hue");
  hsvFolder.add( uniforms, "saturation", -1.0, 1.0, step ).name("Saturation");
  hsvFolder.add( uniforms, "value", -1.0, 1.0, step ).name("Value");

  // Create a vertex array object (attribute state)
  var vao = gl.createVertexArray();

  // and make it the one we're currently working with
  gl.bindVertexArray(vao);

  // Create a buffer and put a single pixel space rectangle in
  // it (2 triangles)
  var positionBuffer = gl.createBuffer();

  // Turn on the attribute
  gl.enableVertexAttribArray(positionAttributeLocation);

  // Bind it to ARRAY_BUFFER (think of it as ARRAY_BUFFER = positionBuffer)
  gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);

  // Tell the attribute how to get data out of positionBuffer (ARRAY_BUFFER)
  var size = 2;          // 2 components per iteration
  var type = gl.FLOAT;   // the data is 32bit floats
  var normalize = false; // don't normalize the data
  var stride = 0;        // 0 = move forward size * sizeof(type) each iteration to get the next position
  var offset = 0;        // start at the beginning of the buffer
  gl.vertexAttribPointer(
      positionAttributeLocation, size, type, normalize, stride, offset);

  // provide texture coordinates for the rectangle.
  var texCoordBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, texCoordBuffer);
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([
      0.0,  0.0,
      1.0,  0.0,
      0.0,  1.0,
      0.0,  1.0,
      1.0,  0.0,
      1.0,  1.0,
  ]), gl.STATIC_DRAW);

  // Turn on the attribute
  gl.enableVertexAttribArray(texCoordAttributeLocation);

  // Tell the attribute how to get data out of texCoordBuffer (ARRAY_BUFFER)
  var size = 2;          // 2 components per iteration
  var type = gl.FLOAT;   // the data is 32bit floats
  var normalize = false; // don't normalize the data
  var stride = 0;        // 0 = move forward size * sizeof(type) each iteration to get the next position
  var offset = 0;        // start at the beginning of the buffer
  gl.vertexAttribPointer(
      texCoordAttributeLocation, size, type, normalize, stride, offset);
  for (var i = 0; i < images.length; ++i)
  {
    // Create a texture.
    var texture = gl.createTexture();

    // make unit 0 the active texture uint
    // (ie, the unit all other texture commands will affect
    gl.activeTexture(gl.TEXTURE0 + i);

    // Bind it to texture unit 0' 2D bind point
    gl.bindTexture(gl.TEXTURE_2D, texture);

    // Set the parameters so we don't need mips and so we're not filtering
    // and we don't repeat
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);

    // Upload the image into the texture.
    var mipLevel = 0;               // the largest mip
    var internalFormat = gl.RGBA;   // format we want in the texture
    var srcFormat = gl.RGBA;        // format of data we are supplying
    var srcType = gl.UNSIGNED_BYTE; // type of data we are supplying
    gl.texImage2D(gl.TEXTURE_2D,
                  mipLevel,
                  internalFormat,
                  srcFormat,
                  srcType,
                  images[i]);
  }
  const render = () =>
  {
    drawScene(false);
  }
  render();

  function drawScene(originalSize, width, height)
  {
      //webglUtils.resizeCanvasToDisplaySize(gl.canvas);

      // Tell WebGL how to convert from clip space to pixels
      if (originalSize)
      {
        canvas.width = width;
        canvas.height = height;
        gl.viewport(0, 0, width, height);
      }
      else if (uniforms.canvasMultiplier)
      {
        webglUtils.resizeCanvasToDisplaySize(gl.canvas);
        gl.viewport(gl.canvas.width / 5, gl.canvas.height / 5, gl.canvas.width / 2, gl.canvas.height / 2);
        //gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
      }
      else
      {
        webglUtils.resizeCanvasToDisplaySize(gl.canvas);
        gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
      }
      
      /*else
      {
        gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
        //downloadImage(gl,);
        download = false;
        //uniforms.canvasMultiplier = true;
      }*/

      // Clear the canvas
      gl.clearColor(0, 0, 0, 0);
      gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

      // Tell it to use our program (pair of shaders)
      gl.useProgram(program);

      // Bind the attribute/buffer set we want.
      gl.bindVertexArray(vao);

      // Pass in the canvas resolution so we can convert from
      // pixels to clipspace in the shader
      gl.uniform2f(resolutionLocation, gl.canvas.width, gl.canvas.height);

      // Tell the shader to get the texture from texture unit 0
      for (var i = 0; i < imageLocations.length; ++i)
      {
        gl.uniform1i(imageLocations[i], i);
      }
      gl.uniform2f(noiseLocation, uniforms.noiseStrength, uniforms.noiseFrequency);
      gl.uniform3f(moveLocation, uniforms.X, uniforms.Y, uniforms.angle);
      gl.uniform1f(softnessLocation, uniforms.softness);
      gl.uniform1i(shapeLocation, uniforms.shape);
      gl.uniform1f(sizeLocation, uniforms.size);
      gl.uniform2f(brightnessContrastLocation, uniforms.brightness, uniforms.contrast);
      gl.uniform3f(hsvLocation, uniforms.hue, uniforms.saturation, uniforms.value);
      gl.uniform1f(widthSDFLocation, uniforms.widthSDF);
      gl.uniform1i(vignetteLocation, uniforms.vignetteDummy ? 1 : 0);
      gl.uniform1i(ringsLocation, uniforms.rings);
      gl.uniform1i(scaleLogoLocation, uniforms.logoScale);
      gl.uniform1f(noiseTimeLocation, uniforms.noiseTime * 1000.0);
      gl.uniform1f(innerWidthLocation, uniforms.innerWidth);

      // Bind the position buffer so gl.bufferData that will be called
      // in setRectangle puts data in the position buffer
      gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);

      // Set a rectangle the same size as the image.
      setRectangle(gl, 0, 0, gl.canvas.width, gl.canvas.height);

      // Draw the rectangle.
      var primitiveType = gl.TRIANGLES;
      var offset = 0;
      var count = 6;
      gl.drawArrays(primitiveType, offset, count);

      window.requestAnimationFrame(render);
  }

  function scaleCheckbox(checkbox)
  {
    checkbox = !checkbox;
  }

  function setRectangle(gl, x, y, width, height) 
  {
    var x1 = x;
    var x2 = x + width;
    var y1 = y;
    var y2 = y + height;
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([
      x1, y1,
      x2, y1,
      x1, y2,
      x1, y2,
      x2, y1,
      x2, y2,
    ]), gl.STATIC_DRAW);
  }

  const elem = document.getElementById('download');
  elem.addEventListener('click', downloadListener, { once: true });
  function downloadListener() 
  {
    var downloadName = imgNames[0].split('.')[0] + "_Edited";
    drawScene(true, images[0].width, images[0].height);
    canvas.toBlob((blob) => 
    {
      saveBlob(blob, `${downloadName}.png`);
    });
    if (uniforms.miniatureDummy)
    {
      drawScene(true, images[0].width/4, images[0].height/4);
      canvas.toBlob((blob) => {
        saveBlob(blob, `${downloadName}_Lazy.png`);
      });
    }
    console.log(`${downloadName}.png`);
    elem.removeEventListener('click', downloadListener);
  };

  const saveBlob = (function() {
    const a = document.createElement('a');
    document.body.appendChild(a);
    a.style.display = 'none';
    return function saveData(blob, fileName) {
       const url = window.URL.createObjectURL(blob);
       a.href = url;
       a.download = fileName;
       a.click();
    };
  }());

  function vignetteCheckbox(vignette)
  {
    if (vignette == 0)
    {
      vignette = 1;
    }
    else
    {
      vignette = 0;
    }
  }

  function miniatureCheckbox(miniature)
  {
    miniature = !miniature;
  }
  //window.addEventListener('load', function() 
  //{
    document.querySelector('input[type="file"]').addEventListener('change', function() 
    {
      if (this.files && this.files[0]) 
      {
        var img = new Image();
        img.onload = () => 
        {
            URL.revokeObjectURL(img.src);  // no longer needed, free memory
        }
        img.src = URL.createObjectURL(this.files[0]); // set src to blob url
        images[0] = img;  // MUST BE SAME DOMAIN!!!
        var name = this.files[0].name.split('.');
        imgNames[0] = name[0];
        //console.log(imgNames[0]);
        images[0].onload = function() 
        {
          elem.removeEventListener('click', downloadListener);
          main();
        };
      }
    }, {once: true});
  //});
}
