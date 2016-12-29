#version 410

out vec4 FragColor;

uniform sampler2D uVelocity;
uniform sampler2D uSource;
uniform sampler2D uObstacles;

uniform vec2 uInverseSize;
uniform float uTimeStep;
uniform float uDissipation;

void main()
{
    vec2 fragCoord = gl_FragCoord.xy;
    float solid = texture(uObstacles, uInverseSize * fragCoord).x;
    if (solid > 0) {
        FragColor = vec4(0);
        return;
    }

    vec2 u = texture(uVelocity, uInverseSize * fragCoord).xy;
    vec2 coord = uInverseSize * (fragCoord - uTimeStep * u);
    FragColor = uDissipation * texture(uSource, coord);
}