#version 410

#extension GL_ARB_shading_language_include : require

#include <primitives.h>
#include <operations.h>
#include <color.h>

uniform float uTime;

in vec2 vUV;

out vec4 FragColor;

float smoothedge(float v) {
    return smoothstep(0.0, 1.0 / 512, v);
}

void main()
{
	float d = rect(vUV - vec2(0.5, 0.5), vec2(0.1, 0.08));

	d = min(d, circle(vUV - vec2(0.8, 0.8), 0.1));

	vec4 color = vec4(1.0, 0.0, 0.0, 1.0);
    FragColor = mix
        ( color
        , vec4(0, 0, 0, 0) // transparent
        , vec4(smoothedge(d)));
}