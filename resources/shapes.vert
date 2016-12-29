#version 330 core

uniform mat4 uTransform;

in vec2 aPosition;
in vec2 aUV;

out vec2 vUV;
void main( void ) {
    gl_Position = uTransform * vec4(aPosition, 0.0, 1.0);
    vUV = aUV;
}
