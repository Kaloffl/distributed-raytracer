
#define EPSILON 0.0001f
#define MAX_DEPTH 1000000000000
#define MAX_BOUNCES 4
#define MIN_ROUGHNESS 0.0001f

#define PI 3.14159265359f

#define f3(x, y, z) (float3)(x, y, z)

typedef struct {
  float3 pos;
  float radius;
} sphere;

typedef struct {
  float3 normal;
  float dist;
} plane;

typedef struct {
  float3 start;
  float3 normal;
} ray;

typedef struct {
  float3 pos;
  float3 right;
  float3 up;
  float3 forward;
} camera;

typedef struct {
  float3 color;
  float emission;
  float roughness;
  float ior;
  float metalness;
  float transparency;
} material;

__constant sphere spheres[] = {
  (sphere) {
    f3(-1.0f, 0.5f, -4.0f), // bos
	0.25f // radius
  },
  (sphere) {
    f3(1.0f, 2.0f, -3.0f), // pos
    0.25f // radius
  },
  (sphere) {
    f3(-1.0f, 4.0f, -2.0f), // pos
    0.5f // radius
  },
  (sphere) {
    f3(-1.0f, 1.0f, 0.0f), // pos
    1.0f // radius
  },
  (sphere) {
    f3(2.0f, 1.0f, -4.0f), // pos
    1.0f // radius
  }
};
__constant material sphere_materials[] = {
  (material) {
    f3(1.0f, 0.5f, 0.5f), // color
    20.0f, // emission
    0.0f, // roughness
    1.42f, // ior
    0.0f, // metalness
    0.0f // transparency
  },
  (material) {
    f3(0.5f, 1.0f, 0.5f), // color
    20.0f, // emission
    0.0f, // roughness
    1.42f, // ior
    0.0f, // metalness
    0.0f // transparency
  },
  (material) {
    f3(0.5f, 0.5f, 1.0f), // color
    20.0f, // emission
    0.0f, // roughness
    1.42f, // ior
    0.0f, // metalness
    0.0f // transparency
  },
  (material) {
    f3(0.89f, 0.78f, 0.74f), // color
    0.0f, // emission
    0.0f, // roughness
    1.42f, // ior
    0.0f, // metalness
    0.0f // transparency
  },
  (material) {
    f3(0.955f, 0.638f, 0.538f), // color
    0.0f, // emission
    0.01f, // roughness
    2.0f, // ior
    1.0f, // metalness
    0.0f // transparency
  }
};
__constant plane planes[] = {
  (plane) {
    f3(1.0f, 0.0f, 0.0f),
    4.0f
  },
  (plane) {
    f3(-1.0f, 0.0f, 0.0f),
    4.0f
  },
  (plane) {
    f3(0.0f, 1.0f, 0.0f),
    0.0f
  },
  (plane) {
    f3(0.0f, -1.0f, 0.0f),
    6.0f
  },
  (plane) {
    f3(0.0f, 0.0f, 1.0f),
    12.0f
  },
  (plane) {
    f3(0.0f, 0.0f, -1.0f),
    4.0f
  }
};
__constant material plane_materials[] = {
  (material) {
    f3(0.9f, 0.1f, 0.1f), // color
    0.0f, // emission
    1.0f, // roughness
    1.42f, // ior
    0.0f, // metalness
    0.0f // transparency
  },
  (material) {
    f3(0.1f, 0.1f, 0.9f), // color
    0.0f, // emission
    1.0f, // roughness
    1.42f, // ior
    0.0f, // metalness
    0.0f // transparency
  },
  (material) {
    f3(0.4f, 0.59f, 0.41f), // color
    0.0f, // emission
    0.0f, // roughness
    1.42f, // ior
    0.0f, // metalness
    0.0f // transparency
  },
  (material) {
    f3(0.9f, 0.9f, 0.9f), // color
    0.0f, // emission
    1.0f, // roughness
    1.42f, // ior
    0.0f, // metalness
    0.0f // transparency
  },
  (material) {
    f3(0.9f, 0.9f, 0.9f), // color
    0.0f, // emission
    1.0f, // roughness
    1.42f, // ior
    0.0f, // metalness
    0.0f // transparency
  },
  (material) {
    f3(0.9f, 0.9f, 0.9f), // color
    0.0f, // emission
    1.0f, // roughness
    1.42f, // ior
    0.0f, // metalness
    0.0f // transparency
  }
};
__constant camera c = {
  f3(0.0f, 1.0f, -8.0f), // pos
  f3(-1.0f, 0.0f, 0.0f),  // right
  f3(0.0f, 1.0f, 0.0f),  // up
  f3(0.0f, 0.0f, 1.0f)   // forward
};

uint next_rand(ulong *seed) {
  uint seed0 = (*seed) & 0xFFFFFFFF;
  uint seed1 = (*seed >> 32) & 0xFFFFFFFF;
  seed1 = 36969 * (seed0 & 65535) + (seed0 >> 16);
  seed0 = 18000 * (seed1 & 65535) + (seed1 >> 16);
  *seed = seed0 | seed1 << 32;
  return seed0 ^ seed1;
}

float next_rand_float(ulong *seed) {
  union {
    uint i;
    float f;
  } u;
  u.i = (next_rand(seed) & 0x007fffff) | 0x40000000;
  return (u.f - 2.0f) / 2.0f;
}

bool intersect_ray_sphere(
    __constant sphere *s,
    const ray *r,
    float *result) {

  float3 a = s->pos - r->start;
  float  b = dot(a, r->normal);
  float  c = dot(a, a) - s->radius * s->radius;
  float disc = b * b - c;

  if (disc < 0.0f) return false;

  float sq = sqrt(disc);
  float depth = b - sq;
  if (depth > EPSILON) {
    *result = depth;
    return true;
  }
  depth = b + sq;
  if (depth > EPSILON) {
    *result = depth;
    return true;
  }
  return false;
}

float3 sphere_normal(__constant sphere *s, float3 p) {
  return (p - s->pos) / s->radius;
}

bool intersect_ray_plane(
    __constant plane *p,
    const ray *r,
    float *result) {

  float d = dot(p->normal, r->normal);
  float t = -(dot(p->normal, r->start) + p->dist) / d;
  if (EPSILON < t) {
    *result = t;
    return true;
  }
  return false;
}

float3 reflect(const float3 v_in, const float3 v_surface){
  return v_in - 2.0f * v_surface * dot(v_in, v_surface);
}

float refractance(
    float3 v_in,
    float3 v_surface,
    float i1,
    float i2) {

  float cos_i = -dot(v_in, v_surface);
  if (0 == cos_i || i1 == i2) return 0;
  float n = i1 / i2;
  float sin_t2 = n * n * (1.0f - cos_i * cos_i);
  if (sin_t2 >= 1.0f) {
    return 1.0f;
  }
  float cos_t = sqrt(1.0f - sin_t2);
  float r_ortho = (i1 * cos_i - i2 * cos_t) / (i1 * cos_i + i2 * cos_t);
  return r_ortho * r_ortho;
}

float3 refract(
    float3 v_in,
    float3 v_surface,
    float i1,
    float i2) {

  float cos_i = -dot(v_in, v_surface);
  if (0 == cos_i || i1 == i2) return v_in;
  float n = i1 / i2;
  float sin_t2 = n * n * (1.0f - cos_i * cos_i);
  if (sin_t2 >= 1.0f) {
    return reflect(v_in, v_surface);
  }
  float a = n * cos_i - sqrt(1 - sin_t2);
  return v_in * n + v_surface * a;
}

float3 evaluate_bsdf(
    material *mat,
    float3 to_eye,
    float3 surface_normal,
    float3 to_light) {

  //return mat->color;

  float rough = max(MIN_ROUGHNESS, mat->roughness);
  float3 reflected = reflect(-to_eye, surface_normal);
  float3 h = normalize(to_eye + to_light);
  float specular_amount = mix(refractance(-to_light, h, 1.0f, mat->ior), 1.0f, mat->metalness);
  float3 reflection_color = mix(f3(1, 1, 1), mat->color, mat->metalness);
  float3 specular = reflection_color / ((rough + 2.0f) / 8.0f * pow(dot(h, surface_normal), rough));
  specular /= dot(to_eye, h);
  
  //if (dot(reflected, to_light) < (1 - rough)) {
  if (dot(surface_normal, h) <= 0.0f/*(1 - rough)*/) {
    specular = f3(0.0f, 0.0f, 0.0f);
  }
  
  float3 diffuse = mat->color;
  return mix(diffuse, specular, specular_amount);
}

float3 random_normal(float r1, float r2) {
  float angle = r1 * 2.0f * PI;
  float rnd = r2 * 2.0f - 1.0f;
  float dist = sqrt(1.0f - rnd * rnd);
  return f3(
    dist * cos(angle),
    dist * sin(angle),
    rnd);
}

float3 cone_sample(float3 base_z, float r1, float r2, float extend, float bias) {
  float s = (base_z.z < 0.0f) ? -1.0f : 1.0f;
  float a = -1.0f / (s + base_z.z);
  float b = base_z.x * base_z.y * a;
  float3 base_x = f3(1.0f + s * base_z.x * base_z.x * a, s * b, -s * base_z.x);
  float3 base_y = f3(b, s + base_z.y * base_z.y * a, -base_z.y);

  float angle = r1 * 2.0f * PI;
  float rnd = pow(1.0f - r2 * extend, 1.0f / (1.0f + bias));
  float dist = sqrt(1.0f - rnd * rnd);
  return base_x * (dist * cos(angle)) +
         base_y * (dist * sin(angle)) +
         base_z * rnd;
}

float3 diffuse_sample(float3 v_surface, float r1, float r2) {
  return cone_sample(v_surface, r1, r2, 1.0f, 1.0f);
}

float diffuse_weight(float3 v_surface, float3 v_out) {
  return dot(v_surface, v_out) * 2.0f;
}

float3 reflection_sample(float3 v_in, float3 v_surface, float r1, float r2, float roughness) {
  float3 reflected = reflect(v_in, v_surface);
  float rough = max(MIN_ROUGHNESS, roughness);
  float3 cone = cone_sample(reflected, r1, r2, rough, 0.0f);
  if (dot(cone, v_surface) < 0) {
    return reflect(cone, v_surface);
  }
  return cone;
}

float reflection_weight(float3 v_in, float3 v_surface, float3 v_out, float roughness) {
  float3 reflected = reflect(v_in, v_surface);
  float rough = max(MIN_ROUGHNESS, roughness);
  if (dot(reflected, v_out) < (1 - rough)) {
    return 0.0f;
  }
  return 1.0f / rough;
}

float3 blinn_sample(float3 v_in, float3 v_surface, float r1, float r2, float roughness) {
  float rough = max(MIN_ROUGHNESS, roughness);
  float3 h = cone_sample(v_surface, r1, r2, rough, 1.0f);
  float3 refl = reflect(v_in, h);
  if (dot(refl, v_surface) < 0.0f) return reflect(v_in, v_surface);
  return refl;
}

float blinn_weight(float3 v_in, float3 v_surface, float3 v_out, float roughness) {
  float3 h = normalize(v_out - v_in);
  float rough = max(MIN_ROUGHNESS, roughness);
  float s_dot_h = dot(v_surface, h);
  if (s_dot_h <= 0.0f) return 0.0f;
  return (rough + 2.0f) / 8.0f * pow(s_dot_h, rough);
}

float3 sphere_sample(float3 pos, __constant sphere *s, float r1, float r2) {
  float3 to_sphere = s->pos - pos;
  float dist_sq = dot(to_sphere, to_sphere);
  float3 to_sphere_normal = normalize(to_sphere);
  float angle = 1.0f - sqrt(1.0f - s->radius * s->radius / dist_sq);
  return cone_sample(to_sphere_normal, r1, r2, angle, 0.0f);
}

float sphere_weight(float3 v_out, float3 pos, __constant sphere *s) {
  float3 to_sphere = s->pos - pos;
  float dist_sq = dot(to_sphere, to_sphere);
  float r_sq = s->radius * s->radius;
  if (r_sq > dist_sq) {
    return 1.0f / 2.0f;
  }
  float3 to_sphere_normal = normalize(to_sphere);
  float angle = 1.0f - sqrt(1.0f - r_sq / dist_sq);
  if (dot(to_sphere_normal, v_out) < (1 - angle)) {
    return 0.0f;
  }
  return 1.0f / angle;
}

float bsdf_weight(material *mat, float3 v_in, float3 v_normal, float3 v_out) {
  float specular_amount = mix(refractance(v_in, v_normal, 1.0, mat->ior), 1.0f, mat->metalness) * (1.0f - mat->roughness);
  float w1 = diffuse_weight(v_normal, v_out);
  float w2 = reflection_weight(v_in, v_normal, v_out, mat->roughness);
  //float w2 = blinn_weight(v_in, v_normal, v_out, mat->roughness);
  return mix(w1, w2, specular_amount);
}

ray make_ray(__constant camera *c, float x, float y) {
  float sx = 0.005f * x;
  float sy = 0.005f * y;
  float sz = 0.005f;
  float dx = c->right.x * sx + c->up.x * sy + c->forward.x * sz;
  float dy = c->right.y * sx + c->up.y * sy + c->forward.y * sz;
  float dz = c->right.z * sx + c->up.z * sy + c->forward.z * sz;
  float dil = 1.0f / sqrt(dx * dx + dy * dy + dz * dz);
  return (ray) {
    c->pos,
    f3(dx * dil, dy * dil, dz * dil)
  };
}

bool intersect_scene(ray *r, material *mat, float *dist, float3 *normal) {
  float d = MAX_DEPTH;
  for (int i = 0; i < 5; ++i) {
    if (intersect_ray_sphere(&spheres[i], r, &d) && d < *dist) { 
      *dist = d;
      *mat = sphere_materials[i];
      *normal = sphere_normal(&spheres[i], r->start + r->normal * d);
    }
  }
  for (int i = 0; i < 6; ++i) {
    if (intersect_ray_plane(&planes[i], r, &d) && d < *dist) { 
      *dist = d;
      *mat = plane_materials[i];
      *normal = planes[i].normal;
    }
  }
  return *dist < MAX_DEPTH;
}

__constant sphere *pick_hint(float *weight, ulong *seed) {
  uint rnd = next_rand(seed) % 3;
  *weight = 1.0f;
  return &spheres[rnd];
}

float3 trace(float x, float y, ulong *seed) {
  float3 color = f3(1, 1, 1);
  float3 result = f3(0, 0, 0);
  ray r = make_ray(&c, x, y);
  for (int bounce = 0; bounce < MAX_BOUNCES; ++bounce) {
    float distance = MAX_DEPTH;
    float3 normal;
    material mat;
    if (intersect_scene(&r, &mat, &distance, &normal)) {
      if (mat.emission > 0.0f) {
        result += color * mat.color * mat.emission;
        break;
      } else {
        float3 pos = r.start + r.normal * distance;
        // direct lighting
        float hint_weight = 0;
        __constant sphere *light_hint = pick_hint(&hint_weight, seed);
        {
          float r1 = next_rand_float(seed);
          float r2 = next_rand_float(seed);
          float3 light_normal = sphere_sample(pos, light_hint, r1, r2);
          float light_w = sphere_weight(light_normal, pos, light_hint) * hint_weight;
          float cosine = dot(light_normal, normal);
          if (cosine > 0.0f) {
            float3 sr = evaluate_bsdf(&mat, -r.normal, normal, light_normal);
			if (dot(sr, sr) > 0.0f) {
              material mat2;
              float distance2 = MAX_DEPTH;
              float3 normal2;
              ray r2 = (ray) {pos, light_normal};
              if (intersect_scene(&r2, &mat2, &distance2, &normal2)) {
			    float3 light = mat2.color * mat2.emission * sr * cosine;
                float bsdf_w = bsdf_weight(&mat, r.normal, normal, light_normal);
				float mis = light_w / (light_w * light_w + bsdf_w * bsdf_w);
                result += color * light * mis;
              }
            }
          }
        }

        // indirect lighting
        float3 next_normal;
        {
          float r1 = next_rand_float(seed);
          float r2 = next_rand_float(seed);
          float r3 = next_rand_float(seed);
          float specular_amount = mix(refractance(r.normal, normal, 1.0, mat.ior), 1.0f, mat.metalness) * (1.0f - mat.roughness);
          if (r1 > specular_amount) {
            next_normal = diffuse_sample(normal, r2, r3);
          } else {
            next_normal = reflection_sample(r.normal, normal, r2, r3, mat.roughness);
            //next_normal = blinn_sample(r.normal, normal, r2, r3, mat.roughness);
          }

          float bsdf_w = bsdf_weight(&mat, r.normal, normal, next_normal);
		  float light_w = sphere_weight(next_normal, pos, light_hint) * hint_weight;
		  float mis = bsdf_w / (light_w * light_w + bsdf_w * bsdf_w);
          float cosine = dot(next_normal, normal);
          color *= evaluate_bsdf(&mat, -r.normal, normal, next_normal) * cosine * mis;
        }
        r.start = pos;
        r.normal = next_normal;
      }
    } else {
      result += color * f3(0.3984f, 0.5117f, 0.7305f) * 0.1f;
      break;
    }

  }
  return result;
}

__kernel void MAIN(int img_width, int img_height, __global float3 *integration, __global int *result, int iteration) {
  const int id = get_global_id(0);
  const int ix = id % img_width;
  const int iy = id / img_width;
  const float x = ((float) (2 * ix - img_width)) / img_width;
  const float y = ((float) (img_height - 2 * iy)) / img_width;
  ulong seed = ((iteration * img_width + ix) * img_height + iy) ^ 0x124567890ABCDEF;
  
  float rx = (next_rand_float(&seed) * 2 - 1) / img_width;
  float ry = (next_rand_float(&seed) * 2 - 1) / img_width;
  float3 color = trace(x + rx, y + ry, &seed);

  integration[id] = (integration[id] * (iteration - 1) + color) / iteration;
  int r = (int) (pow(min(1.0f, integration[id].x), 1.0f / 2.2f) * 255);
  int g = (int) (pow(min(1.0f, integration[id].y), 1.0f / 2.2f) * 255);
  int b = (int) (pow(min(1.0f, integration[id].z), 1.0f / 2.2f) * 255);
  result[id] = r << 16 | g << 8 | b;
}