#version 410

out vec2 FragColor;
uniform sampler2D uBVelocity;
uniform sampler2D uTemperature;
uniform sampler2D uDensity;
uniform float uAmbientTemperature;
uniform float uBTimeStep;
uniform float uSigma;
uniform float uKappa;

void main()
{
    ivec2 TC = ivec2(gl_FragCoord.xy);
    float T = texelFetch(uTemperature, TC, 0).r;
    vec2 V = texelFetch(uBVelocity, TC, 0).xy;

    FragColor = V;

    if (T > uAmbientTemperature) {
        float D = texelFetch(uDensity, TC, 0).x;
        FragColor += (uBTimeStep * (T - uAmbientTemperature) * uSigma - D * uKappa ) * vec2(0, 1);
    }
}