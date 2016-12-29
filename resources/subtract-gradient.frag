#version 410

out vec2 FragColor;

uniform sampler2D uSGVelocity;
uniform sampler2D uSGPressure;
uniform sampler2D uSGObstacles;
uniform float uGradientScale;

void main()
{
    ivec2 T = ivec2(gl_FragCoord.xy);

    vec3 oC = texelFetch(uSGObstacles, T, 0).xyz;
    if (oC.x > 0) {
        FragColor = oC.yz;
        return;
    }

    // Find neighboring pressure:
    float pN = texelFetchOffset(uSGPressure, T, 0, ivec2(0, 1)).r;
    float pS = texelFetchOffset(uSGPressure, T, 0, ivec2(0, -1)).r;
    float pE = texelFetchOffset(uSGPressure, T, 0, ivec2(1, 0)).r;
    float pW = texelFetchOffset(uSGPressure, T, 0, ivec2(-1, 0)).r;
    float pC = texelFetch(uSGPressure, T, 0).r;

    // Find neighboring obstacles:
    vec3 oN = texelFetchOffset(uSGObstacles, T, 0, ivec2(0, 1)).xyz;
    vec3 oS = texelFetchOffset(uSGObstacles, T, 0, ivec2(0, -1)).xyz;
    vec3 oE = texelFetchOffset(uSGObstacles, T, 0, ivec2(1, 0)).xyz;
    vec3 oW = texelFetchOffset(uSGObstacles, T, 0, ivec2(-1, 0)).xyz;

    // Use center pressure for solid cells:
    vec2 obstV = vec2(0);
    vec2 vMask = vec2(1);

    if (oN.x > 0) { pN = pC; obstV.y = oN.z; vMask.y = 0; }
    if (oS.x > 0) { pS = pC; obstV.y = oS.z; vMask.y = 0; }
    if (oE.x > 0) { pE = pC; obstV.x = oE.y; vMask.x = 0; }
    if (oW.x > 0) { pW = pC; obstV.x = oW.y; vMask.x = 0; }

    // Enforce the free-slip boundary condition:
    vec2 oldV = texelFetch(uSGVelocity, T, 0).xy;
    vec2 grad = vec2(pE - pW, pN - pS) * uGradientScale;
    vec2 newV = oldV - grad;
    FragColor = (vMask * newV) + obstV;  
}