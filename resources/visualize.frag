#version 410
#extension GL_ARB_shading_language_include : require

#include <color.h>

out vec4 FragColor;
uniform sampler2D uSampler;
uniform vec3 uFillColor;
uniform vec2 uScale;
uniform int uIsVelocity;

void main()
{
    vec4 value = texture(uSampler, gl_FragCoord.xy * uScale);

    if (uIsVelocity == 1) {
        // Velocity coloring
        FragColor = vec4(value.r, value.g, 0, value.r+value.g);
    } else {
        float L = value.r;
        FragColor = vec4(uFillColor, L);

        // Hue cycling color
        // vec3 col = hsv2rgb_smooth( vec3(L, 0.8, 0.9)  );
        // FragColor = vec4(col, L);
    }
}