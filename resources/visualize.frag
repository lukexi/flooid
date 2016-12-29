#version 410

out vec4 FragColor;
uniform sampler2D uSampler;
uniform vec3 uFillColor;
uniform vec2 uScale;

void main()
{
    float L = texture(uSampler, gl_FragCoord.xy * uScale).r;
    FragColor = vec4(uFillColor, L);
}