#version 410

out vec4 FragColor;

uniform vec2 uPoint;
uniform float uRadius;
uniform vec3 uSplatFillColor;

void main()
{
    float d = distance(uPoint, gl_FragCoord.xy);
    if (d < uRadius) {
        float a = (uRadius - d) * 0.5;
        a = min(a, 1.0);
        FragColor = vec4(uSplatFillColor, a);
    } else {
        FragColor = vec4(0);
    }
}