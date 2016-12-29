#version 410

out vec4 FragColor;

uniform sampler2D uJCPressure;
uniform sampler2D uJCDivergence;
uniform sampler2D uJCObstacles;

uniform float uAlpha;
uniform float uInverseBeta;

void main()
{
    ivec2 T = ivec2(gl_FragCoord.xy);

    // Find neighboring pressure:
    vec4 pN = texelFetchOffset(uJCPressure, T, 0, ivec2(0, 1));
    vec4 pS = texelFetchOffset(uJCPressure, T, 0, ivec2(0, -1));
    vec4 pE = texelFetchOffset(uJCPressure, T, 0, ivec2(1, 0));
    vec4 pW = texelFetchOffset(uJCPressure, T, 0, ivec2(-1, 0));
    vec4 pC = texelFetch(uJCPressure, T, 0);

    // Find neighboring obstacles:
    vec3 oN = texelFetchOffset(uJCObstacles, T, 0, ivec2(0, 1)).xyz;
    vec3 oS = texelFetchOffset(uJCObstacles, T, 0, ivec2(0, -1)).xyz;
    vec3 oE = texelFetchOffset(uJCObstacles, T, 0, ivec2(1, 0)).xyz;
    vec3 oW = texelFetchOffset(uJCObstacles, T, 0, ivec2(-1, 0)).xyz;

    // Use center pressure for solid cells:
    if (oN.x > 0) pN = pC;
    if (oS.x > 0) pS = pC;
    if (oE.x > 0) pE = pC;
    if (oW.x > 0) pW = pC;

    vec4 bC = texelFetch(uJCDivergence, T, 0);
    FragColor = (pW + pE + pS + pN + uAlpha * bC) * uInverseBeta;
}
