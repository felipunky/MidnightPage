"use strict";

var vertexShaderSource = `#version 300 es

// an attribute is an input (in) to a vertex shader.
// It will receive data from a buffer
in vec2 a_position;
in vec2 a_texCoord;

// Used to pass in the resolution of the canvas
uniform vec2 u_Resolution;

// Used to pass the texture coordinates to the fragment shader
out vec2 v_texCoord;

// all shaders have a main function
void main() {

  // convert the position from pixels to 0.0 to 1.0
  vec2 zeroToOne = a_position / u_Resolution;

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
uniform sampler2D u_Channel0;

// the texCoords passed in from the vertex shader.
in vec2 v_texCoord;

// Used to pass in the resolution of the canvas
uniform int u_Frame;
uniform float u_Time;
uniform vec2 u_Resolution;
uniform vec3 u_Mouse;

// we need to declare an output for the fragment shader
out vec4 outColor;

// musk's lense flare, modified by icecool.
// "Modularized" by SolarLiner, with improvements
// See the original at: https://www.shadertoy.com/view/4sX3Rs 

#define ORB_FLARE_COUNT	6
#define DISTORTION_BARREL 1.

mat2 rot(const in float a)
{
    vec2 sinCos = vec2( sin( a ), cos( a ) );
    return mat2(sinCos.y, -sinCos.x,
                sinCos.x,  sinCos.y);
}

void rot(const in vec2 a, out mat2 rot0, out mat2 rot1)
{
    vec2 aSin = sin(a);
    vec2 aSinN = -aSin;
    vec2 aCos = cos(a);
    rot0 = mat2(aCos.x, aSinN.x,
                aSin.x, aCos.x);
    rot1 = mat2(aCos.y, aSinN.y,
                aSin.y, aCos.y);
}

// Courtesy of Iñigo Quilez.
float noise( in vec3 x, sampler2D iChannel0 )
{

    vec3 p = floor(x);
    vec3 f = fract(x);
    f = f*f*(3.0-2.0*f);

    vec2 uv = (p.xy+vec2(37.0,17.0)*p.z) + f.xy;
    vec2 rg = textureLod( iChannel0, (uv+ 0.5)/256.0, 0.0 ).yx;
    return mix( rg.x, rg.y, f.z );

}

float sdMoon(vec2 p, float d, float ra, float rb )
{
    p.y = abs(p.y);
    float a = (ra*ra - rb*rb + d*d)/(2.0*d);
    float b = sqrt(max(ra*ra-a*a,0.0));
    if( d*(p.x*b-p.y*a) > d*d*max(b-p.y,0.0) )
    {
      return length(p-vec2(a,b));
    }
    vec4 pVec4 = vec4(p, p)-vec4(0., 0., d, 0.);
    vec2 dotPVec4 = sqrt(vec2(dot(pVec4.xy, pVec4.xy), dot(pVec4.zw, pVec4.zw)))-vec2(ra, rb);
    return max(dotPVec4.x, -dotPVec4.y);
}

float sdCross( in vec2 p, in vec2 b, float r ) 
{
    p = abs(p); p = (p.y>p.x) ? p.yx : p.xy;
    vec2  q = p - b;
    float k = max(q.y,q.x);
    vec2  w = (k>0.0) ? q : vec2(b.y-p.x,-k);
    return sign(k)*length(max(w,0.0)) + r;
}


vec4 shape(const in vec2 uv, const in vec2 mou, 
           const in vec2 iResolution, const in float iTime, 
           const in sampler2D iChannel0, out mat2 outRot)
{
    float ra = 0.9;
    float rb = 0.75;
    float di = -0.2;
    
    vec2 ang = iTime * vec2(.4, .25);

    vec2 uvN = uv;
    uvN = uvN * 3. + ang.x;

    float g = noise( vec3(uvN, 1), u_Channel0 );// * 3. + ang.x, u_Channel0 );
    
    mat2 r0, r1;
    
    ang.x = cos(ang.x)*.5+.5;
    ang.x *= 0.25+radians(45.);
    rot(ang, r0, r1);
    
    vec4 pMC = uv.xyxy - vec4(0., 0., mou);
    pMC += g * vec2(.05, .01).xxyy;
    pMC.xy *= r0;
    pMC.zw *= r1;
    
    float m = sdMoon( pMC.xy, di, ra, rb );
    float c = sdCross( pMC.zw, vec2(0.3, .15), 0.2 );

    float invR = 3. / iResolution.y;
    
    vec2 dVec2 = ( vec2( m, c ) );
    
    float minMC = min(dVec2.x, dVec2.y);
    float minMCOut = minMC;
    
    float d = minMC-.04;
    //d -= 0.02;
    d = d / (fwidth(d) / invR);
    float s = smoothstep(-invR, invR, d);
    outRot = r1;
    
    return vec4(d, s, minMCOut, g);
}

vec3 rgbSplit(const in vec3 sD)
{
    float r0 = sD.z-.035;
    float rep = 1./2.;
    float hRep = rep * 0.5;

    r0 = mod(r0, hRep)-hRep;

    float rr = exp(-16.*sD.z)*sD.y;
    
    float absD = 0.9;
    float st = .25;
    vec3 ID = r0 + hRep * ( vec3( absD ) - st * vec3(0, 1, 2) );
    ID = abs(ID)-(hRep*.05);

    float sL = 0.333*hRep;
    
    vec3 lS = smoothstep(vec3(0), vec3(sL), ID);
    
    vec3 colL = mix(vec3(0.992,0.286,0.533), vec3(0, 0, 0), lS.z);
    colL      = mix(vec3(0.992,0.800,0.388), colL, lS.y);
    colL      = mix(vec3(0.525,0.918,0.996), colL, lS.x);
    return colL*rr;
}

vec2 GetDistOffset(vec2 uv, vec2 pxoffset)
{
    vec2 tocenter = uv;
    vec3 prep = normalize(vec3(tocenter.y, -tocenter.x, 0.));
    
    float angle = length(tocenter.xy)*2.221*DISTORTION_BARREL;
    vec3 oldoffset = vec3(pxoffset, 0.0);
    vec2 sinCos = vec2( sin( angle ), cos( angle ) );
    
    vec3 rotated = oldoffset * sinCos.y + 
                   cross( prep, oldoffset ) * sinCos.x + 
                   prep * dot( prep, oldoffset ) * ( 1.0 - sinCos.y );
    
    return rotated.xy;
}

float dot2(const in vec2 a)
{
    return dot(a, a);
}

vec3 flare(vec2 uv, vec2 pos, float dist, float size)
{
    float sizeInv = 1./(size*2.);
    pos = GetDistOffset(uv, pos);
    
    vec4 uvPlusDist = uv.xyxy + (dist + vec2(-.05, .05).xxyy) * pos.xyxy;
    
    vec3 uvDot2 = sqrt(vec3(dot2(uvPlusDist.xy), 
                            dot2(uv + dist * pos), 
                            dot2(uvPlusDist.zw)));
    uvDot2 = .01-pow(uvDot2, vec3(2.4))*sizeInv;
    uvDot2 = max(uvDot2, vec3(0.)) * 6.;
    
    return uvDot2;
}
vec3 flare(vec2 uv, vec2 pos, float dist, float size, vec3 color)
{
    return flare(uv, pos, dist, size)*color;
}

vec3 orb(vec2 uv, vec2 pos, float dist, float size)
{
    vec3 c = vec3(0.0);
    
    for(int i=0; i<ORB_FLARE_COUNT; i++)
    {
        float j = float(i+1);
        float offset = j/(j+1.);
        float colOffset = j/float(ORB_FLARE_COUNT*2);
        
        c += flare(uv,pos,dist+offset, size/(j+.1), vec3(1.0-colOffset, 1.0, 0.5+colOffset));
    }
    
    c += flare(uv,pos,dist+.5, 4.0*size, vec3(1.0));
    
    return c;
}
vec3 orb(vec2 uv, vec2 pos, float dist, float size, vec3 color)
{
    return orb(uv,pos,dist,size)*color;
}

vec3 ring(vec2 uv, vec2 pos, float dist)
{
    vec2 uvd = uv*(length(uv));
    
    vec4 uvdPlusDist = uvd.xyxy + (dist + vec2(-.05, .05).xxyy) * pos.xyxy;
    vec3 uvdSqrt = sqrt(vec3(dot2(uvdPlusDist.xy), 
                             dot2(uvd + dist * pos), 
                             dot2(uvdPlusDist.zw)));
    vec3 rgb = max( vec3( 0. ), 1. / (1. + 32. * pow( uvdSqrt, vec3( 2.) ) ) ) * vec3( 0.25, 0.23, 0.21 );
    return rgb;
}

float glare(vec2 uv, vec2 pos, float size, const in mat2 r0, const in float n)
{
    vec2 main = uv-pos + n * .01;
    //main += n * 0.01;
    main *= r0;
	
	float ang = atan( main.y, main.x ) * 1.;
    float r = length( main );
    //main = vec2( sin( ang ), cos( ang ) ).yx * r;
    
	float dist = sdCross( main, vec2(0.3, .15), 0.2 );//length( main ); 
    float distNonPow = dist;
    dist = pow( max( dist, 0.001 ), .1 );
	
	float f0 = 1.0 / ( distNonPow * ( 1.0 / size * 16.0 ) + 3.141559 );
    
    f0 = f0 + f0 * ( cos( ( ang ) * 8.0 ) * .5 + dist );
    return pow(f0, 4.);
}

vec3 lensflare(vec2 uv,vec2 pos, float brightness, float size)
{
    mat2 r0;
    vec4 sD = shape(uv, pos, u_Resolution.xy, u_Time, u_Channel0, r0);
    
    vec3 c = vec3(0);
    c += vec3( 1. - sD.y );
    c += glare( uv, pos, size, r0, sD.w );// * sD.y;
    
    c += flare(uv,pos, -3., 3.*size);
    c += flare(uv,pos, -1., size) * 3.;
    c += flare(uv,pos, .5, .8*size);
    c += flare(uv,pos,-.4, .8*size);
    
    c += orb(  uv,pos,  0.,.5*size);
    
    c += ring(uv,pos,-1.)*.5*size;
    c += ring(uv,pos, 1.)*.5*size;
    
    c = c * brightness;
    
    vec3 rgb = rgbSplit(sD.xyz);
    return c + rgb;
}

void main() 
{

    vec2 p = (-u_Resolution.xy + 2.*gl_FragCoord.xy) / u_Resolution.y;
    vec2 mou = (-u_Resolution.xy + 2.*u_Mouse.xy) / u_Resolution.y;
    if (u_Mouse.z < 1e-3)
    {
        mou = vec2(sin(u_Time*0.1), cos(u_Time*.05));
        mou.x *= 2.;
    }
    vec4 pMou = vec4(p, mou)*1.444;
    p = pMou.xy;
    mou = pMou.zw;
    
    vec3 l = lensflare(p, mou, 1., 3.);
    l = pow(l, vec3(1.222));
    
    outColor = vec4(l, 1.0);
}
`;

function getSize(multiplier, widthImg, heightImg)
{
  const ratio = widthImg > heightImg ? heightImg / widthImg : widthImg / heightImg;
  multiplier = multiplier || 1;
  let width;  
  let height; 
  if (window.innerWidth < widthImg)
  {
    width  = window.innerWidth * multiplier | 0;
    height = window.innerWidth * ratio * multiplier | 0;
  }
  else
  {
    width = widthImg * multiplier | 0;
    height = heightImg * multiplier | 0;
  }
  //console.log(`Get Size Width: ${width} Height: ${height} Ratio: ${ratio} ImageWidth: ${widthImg} ImageHeight: ${heightImg}`);
  return [width, height];
}

const image = new Image();
image.src = "Images/Noise258.png";

image.addEventListener("load", (event) =>
{
    main();
});

async function main()
{
  // Get A WebGL context
  /** @type {HTMLCanvasElement} */
  var canvas = document.getElementById("canvas");

  var size = getSize(window.devicePixelRatio, canvas.width, canvas.height);
  canvas.width = size[0];
  canvas.height = size[1];

  var gl = canvas.getContext("webgl2", { alpha: false });
  if (!gl) 
  {
    return;
  }
  //gl.canvas.width = canvas.width;
  //gl.canvas.height = canvas.height;
  console.log(`Width: ${gl.canvas.width}, Height: ${gl.canvas.height}`);
  //canvas.width = gl.canvas.width;
  //canvas.height = gl.canvas.height;
  // setup GLSL program
  var program = webglUtils.createProgramFromSources(gl,
      [vertexShaderSource, fragmentShaderSource]);

  // look up where the vertex data needs to go.
  var positionAttributeLocation = gl.getAttribLocation(program, "a_position");
  var texCoordAttributeLocation = gl.getAttribLocation(program, "a_texCoord");

  // lookup uniforms
  var imageLocation = gl.getUniformLocation(program, "u_Channel0");
  var resolutionLocation = gl.getUniformLocation(program, "u_Resolution");
  var frameLocation = gl.getUniformLocation(program, "u_Frame");
  var timeLocation = gl.getUniformLocation(program, "u_Time");
  var mouseLocation = gl.getUniformLocation(program, "u_Mouse");

  const uniforms = 
  {
    frame: 0,
    mouseX: 0,
    mouseY: 0,
    mouseClick: 0,
    time: 0.0000
  }

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
  gl.vertexAttribPointer(texCoordAttributeLocation, size, type, normalize, stride, offset);

  function createImageTexture()
  {
    // Create a texture.
    var texture = gl.createTexture();
    // make unit 0 the active texture uint
    // (ie, the unit all other texture commands will affect
    gl.activeTexture(gl.TEXTURE0 + 0);

    // Bind it to texture unit 0' 2D bind point
    gl.bindTexture(gl.TEXTURE_2D, texture);
    //gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
    // Set the parameters so we don't need mips and so we're not filtering
    // and we don't repeat
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR);
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
                image);
    gl.generateMipmap(gl.TEXTURE_2D);
    return texture;
  }

  var imageTexture = createImageTexture();

  const render = () =>
  {
    uniforms.frame += 1;
    uniforms.time += 1./100.;
    drawScene();
  }
  render();

  function drawQuad()
  {
    // Tell it to use our program (pair of shaders)
    gl.useProgram(program);

    // Bind the attribute/buffer set we want.
    gl.bindVertexArray(vao);

    // Pass in the canvas resolution so we can convert from
    // pixels to clipspace in the shader
    gl.uniform1i(imageLocation, 0);
    gl.uniform1i(frameLocation, uniforms.frame);
    gl.uniform1f(timeLocation, uniforms.time);
    gl.uniform2f(resolutionLocation, gl.canvas.width, gl.canvas.height);
    gl.uniform3f(mouseLocation, uniforms.mouseX, uniforms.mouseY, uniforms.mouseClick);
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
  }

  function drawScene()
  {
    {
      let size = getSize(window.devicePixelRatio, window.innerWidth, window.innerHeight/2);
      canvas.style.width = size[0]; 
      canvas.style.height = size[1];
      gl.canvas.width = size[0];
      gl.canvas.height = size[1];
      //console.log(`X: ${gl.canvas.width} Y: ${gl.canvas.height}`);
    }

    // Render to canvas.
    {
      gl.activeTexture(gl.TEXTURE0 + 0);
      //gl.bindTexture(gl.TEXTURE_2D, imageTexture);
      gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
      gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);

      // Clear the canvas
      gl.clearColor(0, 0, 0, 0);
      gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

      drawQuad();
    }
    //console.log(`Mouse X: ${uniforms.mouseX} Mouse Y: ${uniforms.mouseY}`);
    window.requestAnimationFrame(render);
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
  canvas.addEventListener("mouseover", function()
  {
    //console.log("Mouse down");
    uniforms.mouseClick = 1;
  });
  canvas.addEventListener("mouseout", function()
  {
    //console.log("Mouse up");
    uniforms.mouseClick = 0;
  });

  canvas.addEventListener("mousemove", event =>
  {
    //if (uniforms.mouseClick == 1)
    {
      let bound = canvas.getBoundingClientRect();

      let x = event.clientX - bound.left - canvas.clientLeft;
      let y = event.clientY - bound.top - canvas.clientTop;

      uniforms.mouseX = x;// / gl.canvas.width;
      uniforms.mouseY = gl.canvas.height - y;// / gl.canvas.height;
      //console.log(`Mouse X: ${uniforms.mouseX} Mouse Y: ${uniforms.mouseY}`);
    }
  });
}

