#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(location = 0) in vec2 inPos;
layout(location = 1) in vec3 inColor;
layout(location = 0) out vec3 outColor;

layout(binding = 0) uniform Rotation { float theta; } rotation;

void main() {
  float cosTheta = cos(rotation.theta);
  float sinTheta = sin(rotation.theta);
  gl_Position =
    vec4
      (inPos.x * cosTheta - inPos.y * sinTheta,
       inPos.x * sinTheta + inPos.y * cosTheta,
       0.0,
       1.0);
  outColor = inColor;
}
