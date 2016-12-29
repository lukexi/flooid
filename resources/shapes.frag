#version 410

#extension GL_ARB_shading_language_include : require

#include <primitives.h>
#include <operations.h>

uniform vec2 uResolution;
uniform vec2 uMouse;
uniform float uTime;
uniform sampler2D uTexture;

in vec2 vUV;

out vec4 fragColor;

float smoothedge(float v) {
    return smoothstep(0.0, 1.0 / uResolution.x, v);
}

void main() {
    // vec2 st = gl_FragCoord.xy/uResolution.xy;
    vec2 st = vUV;
    // float d = 0.0;
    float d = rect(st - vec2(0.5, 0.2), vec2(0.1, 0.08));
    d = min(d, rect(st - vec2(0.5, 0.2), vec2(0.1, 0.08)));
    d = min(d, roundRect(st - vec2(0.8, 0.2), vec2(0.08, 0.06), 0.02));
    d = min(d, ring(st - vec2(0.2, 0.5), 0.18, 0.02));
    d = min(d, hexagon(st - vec2(0.5, 0.5), 0.1));
    d = min(d, triangle(st - vec2(0.8, 0.5), 0.1));
    d = min(d, ellipse(st - vec2(0.2, 0.8), vec2(0.9, 1.2), 0.1));
    d = min(d, capsule(st - vec2(0.5, 0.8), vec2(-0.05, -0.05), vec2(0.05, 0.05), 0.05));
    d = min(d, polygon(st - vec2(0.8, 0.8), 5, 0.1));

    float circleD = circle(st - vec2(uMouse / uResolution), 0.1);

    // d = fOpUnionRound(circleD, d, 0.05);
    d = fOpUnionRound(circleD, d, 0.5);
    // d = fOpDifferenceRound(circleD, d, 0.05);
    // d = fOpUnionStairs(circleD, d, 0.05, 10);

    // Flip y for texture lookup
    vec2 texUV = vec2(st.x, 1 - st.y);
    vec4 texColor = texture(uTexture, texUV);
    fragColor = mix
        ( texColor
        , vec4(0.9, 0.8, 0.7, 0.0) // transparent
        , vec4(smoothedge(d)));

    // fragColor = texture(uTexture, st);
}

