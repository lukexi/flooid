#version 410

out float FragColor;

uniform sampler2D uCDVelocity;
uniform sampler2D uCDObstacles;
uniform float uHalfInverseCellSize;

void main()
{
    ivec2 T = ivec2(gl_FragCoord.xy);

    // Find neighboring velocities:
    vec2 vN = texelFetchOffset(uCDVelocity, T, 0, ivec2(0, 1)).xy;
    vec2 vS = texelFetchOffset(uCDVelocity, T, 0, ivec2(0, -1)).xy;
    vec2 vE = texelFetchOffset(uCDVelocity, T, 0, ivec2(1, 0)).xy;
    vec2 vW = texelFetchOffset(uCDVelocity, T, 0, ivec2(-1, 0)).xy;

    // Find neighboring obstacles:
    vec3 oN = texelFetchOffset(uCDObstacles, T, 0, ivec2(0, 1)).xyz;
    vec3 oS = texelFetchOffset(uCDObstacles, T, 0, ivec2(0, -1)).xyz;
    vec3 oE = texelFetchOffset(uCDObstacles, T, 0, ivec2(1, 0)).xyz;
    vec3 oW = texelFetchOffset(uCDObstacles, T, 0, ivec2(-1, 0)).xyz;

    // Use obstacle velocities for solid cells:
    if (oN.x > 0) vN = oN.yz;
    if (oS.x > 0) vS = oS.yz;
    if (oE.x > 0) vE = oE.yz;
    if (oW.x > 0) vW = oW.yz;

    FragColor = uHalfInverseCellSize * (vE.x - vW.x + vN.y - vS.y);
}