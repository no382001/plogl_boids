% Definitions and keys from gl.h

%typedef double		GLclampd;	/* double precision float in [0,1] */
%typedef double		GLdouble;	/* double precision float */
%typedef float		GLclampf;	/* single precision float in [0,1] */
%typedef float		GLfloat;	/* single precision float */
%typedef int		GLint;		/* 4-byte signed */
%typedef int		GLsizei;	/* 4-byte signed */
%typedef short		GLshort;	/* 2-byte signed */
%typedef signed char	GLbyte;		/* 1-byte signed */
%typedef unsigned char	GLboolean;
%typedef unsigned char	GLubyte;	/* 1-byte unsigned */
%typedef unsigned int	GLbitfield;
%typedef unsigned int	GLenum;
%typedef unsigned int	GLuint;		/* 4-byte unsigned */
%typedef unsigned short	GLushort;	/* 2-byte unsigned */
%typedef void		GLvoid;


% Version
kGL_VERSION_1_1(1).
kGL_VERSION_1_2(1).
kGL_VERSION_1_3(1).
kGL_VERSION_1_4(1).
kGL_VERSION_1_5(1).
kGL_VERSION_2_0(1).
kGL_VERSION_2_1(1).


% AccumOp
kGL_ACCUM(0x0100).
kGL_LOAD(0x0101).
kGL_RETURN(0x0102).
kGL_MULT(0x0103).
kGL_ADD(0x0104).


% AlphaFunction
kGL_NEVER(0x0200).
kGL_LESS(0x0201).
kGL_EQUAL(0x0202).
kGL_LEQUAL(0x0203).
kGL_GREATER(0x0204).
kGL_NOTEQUAL(0x0205).
kGL_GEQUAL(0x0206).
kGL_ALWAYS(0x0207).


% AttribMask
kGL_CURRENT_BIT(0x00000001).
kGL_POINT_BIT(0x00000002).
kGL_LINE_BIT(0x00000004).
kGL_POLYGON_BIT(0x00000008).
kGL_POLYGON_STIPPLE_BIT(0x00000010).
kGL_PIXEL_MODE_BIT(0x00000020).
kGL_LIGHTING_BIT(0x00000040).
kGL_FOG_BIT(0x00000080).
kGL_DEPTH_BUFFER_BIT(0x00000100).
kGL_ACCUM_BUFFER_BIT(0x00000200).
kGL_STENCIL_BUFFER_BIT(0x00000400).
kGL_VIEWPORT_BIT(0x00000800).
kGL_TRANSFORM_BIT(0x00001000).
kGL_ENABLE_BIT(0x00002000).
kGL_COLOR_BUFFER_BIT(0x00004000).
kGL_HINT_BIT(0x00008000).
kGL_EVAL_BIT(0x00010000).
kGL_LIST_BIT(0x00020000).
kGL_TEXTURE_BIT(0x00040000).
kGL_SCISSOR_BIT(0x00080000).
kGL_ALL_ATTRIB_BITS(0x000fffff).


% BeginMode
kGL_POINTS(0x0000).
kGL_LINES(0x0001).
kGL_LINE_LOOP(0x0002).
kGL_LINE_STRIP(0x0003).
kGL_TRIANGLES(0x0004).
kGL_TRIANGLE_STRIP(0x0005).
kGL_TRIANGLE_FAN(0x0006).
kGL_QUADS(0x0007).
kGL_QUAD_STRIP(0x0008).
kGL_POLYGON(0x0009).


% BlendingFactorDest
kGL_ZERO(0).
kGL_ONE(1).
kGL_SRC_COLOR(0x0300).
kGL_ONE_MINUS_SRC_COLOR(0x0301).
kGL_SRC_ALPHA(0x0302).
kGL_ONE_MINUS_SRC_ALPHA(0x0303).
kGL_DST_ALPHA(0x0304).
kGL_ONE_MINUS_DST_ALPHA(0x0305).
kGL_DST_COLOR(0x0306).
kGL_ONE_MINUS_DST_COLOR(0x0307).
kGL_SRC_ALPHA_SATURATE(0x0308).


% Boolean
kGL_FALSE(0).
kGL_TRUE(1).


% ClipPlaneName
kGL_CLIP_PLANE0(0x3000).
kGL_CLIP_PLANE1(0x3001).
kGL_CLIP_PLANE2(0x3002).
kGL_CLIP_PLANE3(0x3003).
kGL_CLIP_PLANE4(0x3004).
kGL_CLIP_PLANE5(0x3005).


% DataType
kGL_BYTE(0x1400).
kGL_UNSIGNED_BYTE(0x1401).
kGL_SHORT(0x1402).
kGL_UNSIGNED_SHORT(0x1403).
kGL_INT(0x1404).
kGL_UNSIGNED_INT(0x1405).
kGL_FLOAT(0x1406).
kGL_2_BYTES(0x1407).
kGL_3_BYTES(0x1408).
kGL_4_BYTES(0x1409).
kGL_DOUBLE(0x140A).


% DrawBufferMode
kGL_NONE(0).
kGL_FRONT_LEFT(0x0400).
kGL_FRONT_RIGHT(0x0401).
kGL_BACK_LEFT(0x0402).
kGL_BACK_RIGHT(0x0403).
kGL_FRONT(0x0404).
kGL_BACK(0x0405).
kGL_LEFT(0x0406).
kGL_RIGHT(0x0407).
kGL_FRONT_AND_BACK(0x0408).
kGL_AUX0(0x0409).
kGL_AUX1(0x040A).
kGL_AUX2(0x040B).
kGL_AUX3(0x040C).


% ErrorCode
kGL_NO_ERROR(0).
kGL_INVALID_ENUM(0x0500).
kGL_INVALID_VALUE(0x0501).
kGL_INVALID_OPERATION(0x0502).
kGL_STACK_OVERFLOW(0x0503).
kGL_STACK_UNDERFLOW(0x0504).
kGL_OUT_OF_MEMORY(0x0505).


% FeedBackMode
kGL_2D(0x0600).
kGL_3D(0x0601).
kGL_3D_COLOR(0x0602).
kGL_3D_COLOR_TEXTURE(0x0603).
kGL_4D_COLOR_TEXTURE(0x0604).


% FeedBackToken
kGL_PASS_THROUGH_TOKEN(0x0700).
kGL_POINT_TOKEN(0x0701).
kGL_LINE_TOKEN(0x0702).
kGL_POLYGON_TOKEN(0x0703).
kGL_BITMAP_TOKEN(0x0704).
kGL_DRAW_PIXEL_TOKEN(0x0705).
kGL_COPY_PIXEL_TOKEN(0x0706).
kGL_LINE_RESET_TOKEN(0x0707).


% FogMode
kGL_EXP(0x0800).
kGL_EXP2(0x0801).

% FrontFaceDirection
kGL_CW(0x0900).
kGL_CCW(0x0901).

% GetMapTarget
kGL_COEFF(0x0A00).
kGL_ORDER(0x0A01).
kGL_DOMAIN(0x0A02).

% GetTarget
kGL_CURRENT_COLOR(0x0B00).
kGL_CURRENT_INDEX(0x0B01).
kGL_CURRENT_NORMAL(0x0B02).
kGL_CURRENT_TEXTURE_COORDS(0x0B03).
kGL_CURRENT_RASTER_COLOR(0x0B04).
kGL_CURRENT_RASTER_INDEX(0x0B05).
kGL_CURRENT_RASTER_TEXTURE_COORDS(0x0B06).
kGL_CURRENT_RASTER_POSITION(0x0B07).
kGL_CURRENT_RASTER_POSITION_VALID(0x0B08).
kGL_CURRENT_RASTER_DISTANCE(0x0B09).
kGL_POINT_SMOOTH(0x0B10).
kGL_POINT_SIZE(0x0B11).
kGL_POINT_SIZE_RANGE(0x0B12).
kGL_POINT_SIZE_GRANULARITY(0x0B13).
kGL_LINE_SMOOTH(0x0B20).
kGL_LINE_WIDTH(0x0B21).
kGL_LINE_WIDTH_RANGE(0x0B22).
kGL_LINE_WIDTH_GRANULARITY(0x0B23).
kGL_LINE_STIPPLE(0x0B24).
kGL_LINE_STIPPLE_PATTERN(0x0B25).
kGL_LINE_STIPPLE_REPEAT(0x0B26).
kGL_LIST_MODE(0x0B30).
kGL_MAX_LIST_NESTING(0x0B31).
kGL_LIST_BASE(0x0B32).
kGL_LIST_INDEX(0x0B33).
kGL_POLYGON_MODE(0x0B40).
kGL_POLYGON_SMOOTH(0x0B41).
kGL_POLYGON_STIPPLE(0x0B42).
kGL_EDGE_FLAG(0x0B43).
kGL_CULL_FACE(0x0B44).
kGL_CULL_FACE_MODE(0x0B45).
kGL_FRONT_FACE(0x0B46).
kGL_LIGHTING(0x0B50).
kGL_LIGHT_MODEL_LOCAL_VIEWER(0x0B51).
kGL_LIGHT_MODEL_TWO_SIDE(0x0B52).
kGL_LIGHT_MODEL_AMBIENT(0x0B53).
kGL_SHADE_MODEL(0x0B54).
kGL_COLOR_MATERIAL_FACE(0x0B55).
kGL_COLOR_MATERIAL_PARAMETER(0x0B56).
kGL_COLOR_MATERIAL(0x0B57).
kGL_FOG(0x0B60).
kGL_FOG_INDEX(0x0B61).
kGL_FOG_DENSITY(0x0B62).
kGL_FOG_START(0x0B63).
kGL_FOG_END(0x0B64).
kGL_FOG_MODE(0x0B65).
kGL_FOG_COLOR(0x0B66).
kGL_DEPTH_RANGE(0x0B70).
kGL_DEPTH_TEST(0x0B71).
kGL_DEPTH_WRITEMASK(0x0B72).
kGL_DEPTH_CLEAR_VALUE(0x0B73).
kGL_DEPTH_FUNC(0x0B74).
kGL_ACCUM_CLEAR_VALUE(0x0B80).
kGL_STENCIL_TEST(0x0B90).
kGL_STENCIL_CLEAR_VALUE(0x0B91).
kGL_STENCIL_FUNC(0x0B92).
kGL_STENCIL_VALUE_MASK(0x0B93).
kGL_STENCIL_FAIL(0x0B94).
kGL_STENCIL_PASS_DEPTH_FAIL(0x0B95).
kGL_STENCIL_PASS_DEPTH_PASS(0x0B96).
kGL_STENCIL_REF(0x0B97).
kGL_STENCIL_WRITEMASK(0x0B98).
kGL_MATRIX_MODE(0x0BA0).
kGL_NORMALIZE(0x0BA1).
kGL_VIEWPORT(0x0BA2).
kGL_MODELVIEW_STACK_DEPTH(0x0BA3).
kGL_PROJECTION_STACK_DEPTH(0x0BA4).
kGL_TEXTURE_STACK_DEPTH(0x0BA5).
kGL_MODELVIEW_MATRIX(0x0BA6).
kGL_PROJECTION_MATRIX(0x0BA7).
kGL_TEXTURE_MATRIX(0x0BA8).
kGL_ATTRIB_STACK_DEPTH(0x0BB0).
kGL_CLIENT_ATTRIB_STACK_DEPTH(0x0BB1).
kGL_ALPHA_TEST(0x0BC0).
kGL_ALPHA_TEST_FUNC(0x0BC1).
kGL_ALPHA_TEST_REF(0x0BC2).
kGL_DITHER(0x0BD0).
kGL_BLEND_DST(0x0BE0).
kGL_BLEND_SRC(0x0BE1).
kGL_BLEND(0x0BE2).
kGL_LOGIC_OP_MODE(0x0BF0).
kGL_INDEX_LOGIC_OP(0x0BF1).
kGL_COLOR_LOGIC_OP(0x0BF2).
kGL_AUX_BUFFERS(0x0C00).
kGL_DRAW_BUFFER(0x0C01).
kGL_READ_BUFFER(0x0C02).
kGL_SCISSOR_BOX(0x0C10).
kGL_SCISSOR_TEST(0x0C11).
kGL_INDEX_CLEAR_VALUE(0x0C20).
kGL_INDEX_WRITEMASK(0x0C21).
kGL_COLOR_CLEAR_VALUE(0x0C22).
kGL_COLOR_WRITEMASK(0x0C23).
kGL_INDEX_MODE(0x0C30).
kGL_RGBA_MODE(0x0C31).
kGL_DOUBLEBUFFER(0x0C32).
kGL_STEREO(0x0C33).
kGL_RENDER_MODE(0x0C40).
kGL_PERSPECTIVE_CORRECTION_HINT(0x0C50).
kGL_POINT_SMOOTH_HINT(0x0C51).
kGL_LINE_SMOOTH_HINT(0x0C52).
kGL_POLYGON_SMOOTH_HINT(0x0C53).
kGL_FOG_HINT(0x0C54).
kGL_TEXTURE_GEN_S(0x0C60).
kGL_TEXTURE_GEN_T(0x0C61).
kGL_TEXTURE_GEN_R(0x0C62).
kGL_TEXTURE_GEN_Q(0x0C63).
kGL_PIXEL_MAP_I_TO_I(0x0C70).
kGL_PIXEL_MAP_S_TO_S(0x0C71).
kGL_PIXEL_MAP_I_TO_R(0x0C72).
kGL_PIXEL_MAP_I_TO_G(0x0C73).
kGL_PIXEL_MAP_I_TO_B(0x0C74).
kGL_PIXEL_MAP_I_TO_A(0x0C75).
kGL_PIXEL_MAP_R_TO_R(0x0C76).
kGL_PIXEL_MAP_G_TO_G(0x0C77).
kGL_PIXEL_MAP_B_TO_B(0x0C78).
kGL_PIXEL_MAP_A_TO_A(0x0C79).
kGL_PIXEL_MAP_I_TO_I_SIZE(0x0CB0).
kGL_PIXEL_MAP_S_TO_S_SIZE(0x0CB1).
kGL_PIXEL_MAP_I_TO_R_SIZE(0x0CB2).
kGL_PIXEL_MAP_I_TO_G_SIZE(0x0CB3).
kGL_PIXEL_MAP_I_TO_B_SIZE(0x0CB4).
kGL_PIXEL_MAP_I_TO_A_SIZE(0x0CB5).
kGL_PIXEL_MAP_R_TO_R_SIZE(0x0CB6).
kGL_PIXEL_MAP_G_TO_G_SIZE(0x0CB7).
kGL_PIXEL_MAP_B_TO_B_SIZE(0x0CB8).
kGL_PIXEL_MAP_A_TO_A_SIZE(0x0CB9).
kGL_UNPACK_SWAP_BYTES(0x0CF0).
kGL_UNPACK_LSB_FIRST(0x0CF1).
kGL_UNPACK_ROW_LENGTH(0x0CF2).
kGL_UNPACK_SKIP_ROWS(0x0CF3).
kGL_UNPACK_SKIP_PIXELS(0x0CF4).
kGL_UNPACK_ALIGNMENT(0x0CF5).
kGL_PACK_SWAP_BYTES(0x0D00).
kGL_PACK_LSB_FIRST(0x0D01).
kGL_PACK_ROW_LENGTH(0x0D02).
kGL_PACK_SKIP_ROWS(0x0D03).
kGL_PACK_SKIP_PIXELS(0x0D04).
kGL_PACK_ALIGNMENT(0x0D05).
kGL_MAP_COLOR(0x0D10).
kGL_MAP_STENCIL(0x0D11).
kGL_INDEX_SHIFT(0x0D12).
kGL_INDEX_OFFSET(0x0D13).
kGL_RED_SCALE(0x0D14).
kGL_RED_BIAS(0x0D15).
kGL_ZOOM_X(0x0D16).
kGL_ZOOM_Y(0x0D17).
kGL_GREEN_SCALE(0x0D18).
kGL_GREEN_BIAS(0x0D19).
kGL_BLUE_SCALE(0x0D1A).
kGL_BLUE_BIAS(0x0D1B).
kGL_ALPHA_SCALE(0x0D1C).
kGL_ALPHA_BIAS(0x0D1D).
kGL_DEPTH_SCALE(0x0D1E).
kGL_DEPTH_BIAS(0x0D1F).
kGL_MAX_EVAL_ORDER(0x0D30).
kGL_MAX_LIGHTS(0x0D31).
kGL_MAX_CLIP_PLANES(0x0D32).
kGL_MAX_TEXTURE_SIZE(0x0D33).
kGL_MAX_PIXEL_MAP_TABLE(0x0D34).
kGL_MAX_ATTRIB_STACK_DEPTH(0x0D35).
kGL_MAX_MODELVIEW_STACK_DEPTH(0x0D36).
kGL_MAX_NAME_STACK_DEPTH(0x0D37).
kGL_MAX_PROJECTION_STACK_DEPTH(0x0D38).
kGL_MAX_TEXTURE_STACK_DEPTH(0x0D39).
kGL_MAX_VIEWPORT_DIMS(0x0D3A).
kGL_MAX_CLIENT_ATTRIB_STACK_DEPTH(0x0D3B).
kGL_SUBPIXEL_BITS(0x0D50).
kGL_INDEX_BITS(0x0D51).
kGL_RED_BITS(0x0D52).
kGL_GREEN_BITS(0x0D53).
kGL_BLUE_BITS(0x0D54).
kGL_ALPHA_BITS(0x0D55).
kGL_DEPTH_BITS(0x0D56).
kGL_STENCIL_BITS(0x0D57).
kGL_ACCUM_RED_BITS(0x0D58).
kGL_ACCUM_GREEN_BITS(0x0D59).
kGL_ACCUM_BLUE_BITS(0x0D5A).
kGL_ACCUM_ALPHA_BITS(0x0D5B).
kGL_NAME_STACK_DEPTH(0x0D70).
kGL_AUTO_NORMAL(0x0D80).
kGL_MAP1_COLOR_4(0x0D90).
kGL_MAP1_INDEX(0x0D91).
kGL_MAP1_NORMAL(0x0D92).
kGL_MAP1_TEXTURE_COORD_1(0x0D93).
kGL_MAP1_TEXTURE_COORD_2(0x0D94).
kGL_MAP1_TEXTURE_COORD_3(0x0D95).
kGL_MAP1_TEXTURE_COORD_4(0x0D96).
kGL_MAP1_VERTEX_3(0x0D97).
kGL_MAP1_VERTEX_4(0x0D98).
kGL_MAP2_COLOR_4(0x0DB0).
kGL_MAP2_INDEX(0x0DB1).
kGL_MAP2_NORMAL(0x0DB2).
kGL_MAP2_TEXTURE_COORD_1(0x0DB3).
kGL_MAP2_TEXTURE_COORD_2(0x0DB4).
kGL_MAP2_TEXTURE_COORD_3(0x0DB5).
kGL_MAP2_TEXTURE_COORD_4(0x0DB6).
kGL_MAP2_VERTEX_3(0x0DB7).
kGL_MAP2_VERTEX_4(0x0DB8).
kGL_MAP1_GRID_DOMAIN(0x0DD0).
kGL_MAP1_GRID_SEGMENTS(0x0DD1).
kGL_MAP2_GRID_DOMAIN(0x0DD2).
kGL_MAP2_GRID_SEGMENTS(0x0DD3).
kGL_TEXTURE_1D(0x0DE0).
kGL_TEXTURE_2D(0x0DE1).
kGL_FEEDBACK_BUFFER_POINTER(0x0DF0).
kGL_FEEDBACK_BUFFER_SIZE(0x0DF1).
kGL_FEEDBACK_BUFFER_TYPE(0x0DF2).
kGL_SELECTION_BUFFER_POINTER(0x0DF3).
kGL_SELECTION_BUFFER_SIZE(0x0DF4).


% GetTextureParameter
kGL_TEXTURE_WIDTH(0x1000).
kGL_TEXTURE_HEIGHT(0x1001).
kGL_TEXTURE_INTERNAL_FORMAT(0x1003).
kGL_TEXTURE_BORDER_COLOR(0x1004).
kGL_TEXTURE_BORDER(0x1005).

% HintMode
kGL_DONT_CARE(0x1100).
kGL_FASTEST(0x1101).
kGL_NICEST(0x1102).


% LightName
kGL_LIGHT0(0x4000).
kGL_LIGHT1(0x4001).
kGL_LIGHT2(0x4002).
kGL_LIGHT3(0x4003).
kGL_LIGHT4(0x4004).
kGL_LIGHT5(0x4005).
kGL_LIGHT6(0x4006).
kGL_LIGHT7(0x4007).


% LightParameter
kGL_AMBIENT(0x1200).
kGL_DIFFUSE(0x1201).
kGL_SPECULAR(0x1202).
kGL_POSITION(0x1203).
kGL_SPOT_DIRECTION(0x1204).
kGL_SPOT_EXPONENT(0x1205).
kGL_SPOT_CUTOFF(0x1206).
kGL_CONSTANT_ATTENUATION(0x1207).
kGL_LINEAR_ATTENUATION(0x1208).
kGL_QUADRATIC_ATTENUATION(0x1209).

% ListMode
kGL_COMPILE(0x1300).
kGL_COMPILE_AND_EXECUTE(0x1301).

% LogicOp
kGL_CLEAR(0x1500).
kGL_AND(0x1501).
kGL_AND_REVERSE(0x1502).
kGL_COPY(0x1503).
kGL_AND_INVERTED(0x1504).
kGL_NOOP(0x1505).
kGL_XOR(0x1506).
kGL_OR(0x1507).
kGL_NOR(0x1508).
kGL_EQUIV(0x1509).
kGL_INVERT(0x150A).
kGL_OR_REVERSE(0x150B).
kGL_COPY_INVERTED(0x150C).
kGL_OR_INVERTED(0x150D).
kGL_NAND(0x150E).
kGL_SET(0x150F).

% MaterialParameter
kGL_EMISSION(0x1600).
kGL_SHININESS(0x1601).
kGL_AMBIENT_AND_DIFFUSE(0x1602).
kGL_COLOR_INDEXES(0x1603).

% MatrixMode
kGL_MODELVIEW(0x1700).
kGL_PROJECTION(0x1701).
kGL_TEXTURE(0x1702).

% PixelCopyType
kGL_COLOR(0x1800).
kGL_DEPTH(0x1801).
kGL_STENCIL(0x1802).

% PixelFormat
kGL_COLOR_INDEX(0x1900).
kGL_STENCIL_INDEX(0x1901).
kGL_DEPTH_COMPONENT(0x1902).
kGL_RED(0x1903).
kGL_GREEN(0x1904).
kGL_BLUE(0x1905).
kGL_ALPHA(0x1906).
kGL_RGB(0x1907).
kGL_RGBA(0x1908).
kGL_LUMINANCE(0x1909).
kGL_LUMINANCE_ALPHA(0x190A).

% PixelType
kGL_BITMAP(0x1A00).

% PolygonMode
kGL_POINT(0x1B00).
kGL_LINE(0x1B01).
kGL_FILL(0x1B02).

% RenderingMode
kGL_RENDER(0x1C00).
kGL_FEEDBACK(0x1C01).
kGL_SELECT(0x1C02).

% ShadingModel
kGL_FLAT(0x1D00).
kGL_SMOOTH(0x1D01).

% StencilOp
kGL_KEEP(0x1E00).
kGL_REPLACE(0x1E01).
kGL_INCR(0x1E02).
kGL_DECR(0x1E03).

% StringName
kGL_VENDOR(0x1F00).
kGL_RENDERER(0x1F01).
kGL_VERSION(0x1F02).
kGL_EXTENSIONS(0x1F03).

% TextureCoordName
kGL_S(0x2000).
kGL_T(0x2001).
kGL_R(0x2002).
kGL_Q(0x2003).

% TextureEnvMode
kGL_MODULATE(0x2100).
kGL_DECAL(0x2101).

% TextureEnvParameter
kGL_TEXTURE_ENV_MODE(0x2200).
kGL_TEXTURE_ENV_COLOR(0x2201).

% TextureEnvTarget
kGL_TEXTURE_ENV(0x2300).

% TextureGenMode
kGL_EYE_LINEAR(0x2400).
kGL_OBJECT_LINEAR(0x2401).
kGL_SPHERE_MAP(0x2402).

% TextureGenParameter
kGL_TEXTURE_GEN_MODE(0x2500).
kGL_OBJECT_PLANE(0x2501).
kGL_EYE_PLANE(0x2502).

% TextureMagFilter
kGL_NEAREST(0x2600).
kGL_LINEAR(0x2601).

% TextureMinFilter
kGL_NEAREST_MIPMAP_NEAREST(0x2700).
kGL_LINEAR_MIPMAP_NEAREST(0x2701).
kGL_NEAREST_MIPMAP_LINEAR(0x2702).
kGL_LINEAR_MIPMAP_LINEAR(0x2703).

% TextureParameterName
kGL_TEXTURE_MAG_FILTER(0x2800).
kGL_TEXTURE_MIN_FILTER(0x2801).
kGL_TEXTURE_WRAP_S(0x2802).
kGL_TEXTURE_WRAP_T(0x2803).

% TextureWrapMode
kGL_CLAMP(0x2900).
kGL_REPEAT(0x2901).

% ClientAttribMask
kGL_CLIENT_PIXEL_STORE_BIT(0x00000001).
kGL_CLIENT_VERTEX_ARRAY_BIT(0x00000002).
kGL_CLIENT_ALL_ATTRIB_BITS(0xffffffff).

% polygon_offset
kGL_POLYGON_OFFSET_FACTOR(0x8038).
kGL_POLYGON_OFFSET_UNITS(0x2A00).
kGL_POLYGON_OFFSET_POINT(0x2A01).
kGL_POLYGON_OFFSET_LINE(0x2A02).
kGL_POLYGON_OFFSET_FILL(0x8037).

% texture
kGL_ALPHA4(0x803B).
kGL_ALPHA8(0x803C).
kGL_ALPHA12(0x803D).
kGL_ALPHA16(0x803E).
kGL_LUMINANCE4(0x803F).
kGL_LUMINANCE8(0x8040).
kGL_LUMINANCE12(0x8041).
kGL_LUMINANCE16(0x8042).
kGL_LUMINANCE4_ALPHA4(0x8043).
kGL_LUMINANCE6_ALPHA2(0x8044).
kGL_LUMINANCE8_ALPHA8(0x8045).
kGL_LUMINANCE12_ALPHA4(0x8046).
kGL_LUMINANCE12_ALPHA12(0x8047).
kGL_LUMINANCE16_ALPHA16(0x8048).
kGL_INTENSITY(0x8049).
kGL_INTENSITY4(0x804A).
kGL_INTENSITY8(0x804B).
kGL_INTENSITY12(0x804C).
kGL_INTENSITY16(0x804D).
kGL_R3_G3_B2(0x2A10).
kGL_RGB4(0x804F).
kGL_RGB5(0x8050).
kGL_RGB8(0x8051).
kGL_RGB10(0x8052).
kGL_RGB12(0x8053).
kGL_RGB16(0x8054).
kGL_RGBA2(0x8055).
kGL_RGBA4(0x8056).
kGL_RGB5_A1(0x8057).
kGL_RGBA8(0x8058).
kGL_RGB10_A2(0x8059).
kGL_RGBA12(0x805A).
kGL_RGBA16(0x805B).
kGL_TEXTURE_RED_SIZE(0x805C).
kGL_TEXTURE_GREEN_SIZE(0x805D).
kGL_TEXTURE_BLUE_SIZE(0x805E).
kGL_TEXTURE_ALPHA_SIZE(0x805F).
kGL_TEXTURE_LUMINANCE_SIZE(0x8060).
kGL_TEXTURE_INTENSITY_SIZE(0x8061).
kGL_PROXY_TEXTURE_1D(0x8063).
kGL_PROXY_TEXTURE_2D(0x8064).

% texture_object
kGL_TEXTURE_PRIORITY(0x8066).
kGL_TEXTURE_RESIDENT(0x8067).
kGL_TEXTURE_BINDING_1D(0x8068).
kGL_TEXTURE_BINDING_2D(0x8069).
kGL_TEXTURE_BINDING_3D(0x806A).

% vertex_array
kGL_VERTEX_ARRAY(0x8074).
kGL_NORMAL_ARRAY(0x8075).
kGL_COLOR_ARRAY(0x8076).
kGL_INDEX_ARRAY(0x8077).
kGL_TEXTURE_COORD_ARRAY(0x8078).
kGL_EDGE_FLAG_ARRAY(0x8079).
kGL_VERTEX_ARRAY_SIZE(0x807A).
kGL_VERTEX_ARRAY_TYPE(0x807B).
kGL_VERTEX_ARRAY_STRIDE(0x807C).
kGL_NORMAL_ARRAY_TYPE(0x807E).
kGL_NORMAL_ARRAY_STRIDE(0x807F).
kGL_COLOR_ARRAY_SIZE(0x8081).
kGL_COLOR_ARRAY_TYPE(0x8082).
kGL_COLOR_ARRAY_STRIDE(0x8083).
kGL_INDEX_ARRAY_TYPE(0x8085).
kGL_INDEX_ARRAY_STRIDE(0x8086).
kGL_TEXTURE_COORD_ARRAY_SIZE(0x8088).
kGL_TEXTURE_COORD_ARRAY_TYPE(0x8089).
kGL_TEXTURE_COORD_ARRAY_STRIDE(0x808A).
kGL_EDGE_FLAG_ARRAY_STRIDE(0x808C).
kGL_VERTEX_ARRAY_POINTER(0x808E).
kGL_NORMAL_ARRAY_POINTER(0x808F).
kGL_COLOR_ARRAY_POINTER(0x8090).
kGL_INDEX_ARRAY_POINTER(0x8091).
kGL_TEXTURE_COORD_ARRAY_POINTER(0x8092).
kGL_EDGE_FLAG_ARRAY_POINTER(0x8093).
kGL_V2F(0x2A20).
kGL_V3F(0x2A21).
kGL_C4UB_V2F(0x2A22).
kGL_C4UB_V3F(0x2A23).
kGL_C3F_V3F(0x2A24).
kGL_N3F_V3F(0x2A25).
kGL_C4F_N3F_V3F(0x2A26).
kGL_T2F_V3F(0x2A27).
kGL_T4F_V4F(0x2A28).
kGL_T2F_C4UB_V3F(0x2A29).
kGL_T2F_C3F_V3F(0x2A2A).
kGL_T2F_N3F_V3F(0x2A2B).
kGL_T2F_C4F_N3F_V3F(0x2A2C).
kGL_T4F_C4F_N3F_V4F(0x2A2D).

% bgra
kGL_BGR(0x80E0).
kGL_BGRA(0x80E1).

% blend_color
kGL_CONSTANT_COLOR(0x8001).
kGL_ONE_MINUS_CONSTANT_COLOR(0x8002).
kGL_CONSTANT_ALPHA(0x8003).
kGL_ONE_MINUS_CONSTANT_ALPHA(0x8004).
kGL_BLEND_COLOR(0x8005).

% blend_minmax
kGL_FUNC_ADD(0x8006).
kGL_MIN(0x8007).
kGL_MAX(0x8008).
kGL_BLEND_EQUATION(0x8009).

% blend_equation_separate
kGL_BLEND_EQUATION_RGB(0x8009).
kGL_BLEND_EQUATION_ALPHA(0x883D).

% blend_subtract
kGL_FUNC_SUBTRACT(0x800A).
kGL_FUNC_REVERSE_SUBTRACT(0x800B).

% color_matrix
kGL_COLOR_MATRIX(0x80B1).
kGL_COLOR_MATRIX_STACK_DEPTH(0x80B2).
kGL_MAX_COLOR_MATRIX_STACK_DEPTH(0x80B3).
kGL_POST_COLOR_MATRIX_RED_SCALE(0x80B4).
kGL_POST_COLOR_MATRIX_GREEN_SCALE(0x80B5).
kGL_POST_COLOR_MATRIX_BLUE_SCALE(0x80B6).
kGL_POST_COLOR_MATRIX_ALPHA_SCALE(0x80B7).
kGL_POST_COLOR_MATRIX_RED_BIAS(0x80B8).
kGL_POST_COLOR_MATRIX_GREEN_BIAS(0x80B9).
kGL_POST_COLOR_MATRIX_BLUE_BIAS(0x80BA).
kGL_POST_COLOR_MATRIX_ALPHA_BIAS(0x80BB).

% color_table
kGL_COLOR_TABLE(0x80D0).
kGL_POST_CONVOLUTION_COLOR_TABLE(0x80D1).
kGL_POST_COLOR_MATRIX_COLOR_TABLE(0x80D2).
kGL_PROXY_COLOR_TABLE(0x80D3).
kGL_PROXY_POST_CONVOLUTION_COLOR_TABLE(0x80D4).
kGL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE(0x80D5).
kGL_COLOR_TABLE_SCALE(0x80D6).
kGL_COLOR_TABLE_BIAS(0x80D7).
kGL_COLOR_TABLE_FORMAT(0x80D8).
kGL_COLOR_TABLE_WIDTH(0x80D9).
kGL_COLOR_TABLE_RED_SIZE(0x80DA).
kGL_COLOR_TABLE_GREEN_SIZE(0x80DB).
kGL_COLOR_TABLE_BLUE_SIZE(0x80DC).
kGL_COLOR_TABLE_ALPHA_SIZE(0x80DD).
kGL_COLOR_TABLE_LUMINANCE_SIZE(0x80DE).
kGL_COLOR_TABLE_INTENSITY_SIZE(0x80DF).

% convolution
kGL_CONVOLUTION_1D(0x8010).
kGL_CONVOLUTION_2D(0x8011).
kGL_SEPARABLE_2D(0x8012).
kGL_CONVOLUTION_BORDER_MODE(0x8013).
kGL_CONVOLUTION_FILTER_SCALE(0x8014).
kGL_CONVOLUTION_FILTER_BIAS(0x8015).
kGL_REDUCE(0x8016).
kGL_CONVOLUTION_FORMAT(0x8017).
kGL_CONVOLUTION_WIDTH(0x8018).
kGL_CONVOLUTION_HEIGHT(0x8019).
kGL_MAX_CONVOLUTION_WIDTH(0x801A).
kGL_MAX_CONVOLUTION_HEIGHT(0x801B).
kGL_POST_CONVOLUTION_RED_SCALE(0x801C).
kGL_POST_CONVOLUTION_GREEN_SCALE(0x801D).
kGL_POST_CONVOLUTION_BLUE_SCALE(0x801E).
kGL_POST_CONVOLUTION_ALPHA_SCALE(0x801F).
kGL_POST_CONVOLUTION_RED_BIAS(0x8020).
kGL_POST_CONVOLUTION_GREEN_BIAS(0x8021).
kGL_POST_CONVOLUTION_BLUE_BIAS(0x8022).
kGL_POST_CONVOLUTION_ALPHA_BIAS(0x8023).
kGL_CONSTANT_BORDER(0x8151).
kGL_REPLICATE_BORDER(0x8153).
kGL_CONVOLUTION_BORDER_COLOR(0x8154).

% draw_range_elements
kGL_MAX_ELEMENTS_VERTICES(0x80E8).
kGL_MAX_ELEMENTS_INDICES(0x80E9).

% histogram
kGL_HISTOGRAM(0x8024).
kGL_PROXY_HISTOGRAM(0x8025).
kGL_HISTOGRAM_WIDTH(0x8026).
kGL_HISTOGRAM_FORMAT(0x8027).
kGL_HISTOGRAM_RED_SIZE(0x8028).
kGL_HISTOGRAM_GREEN_SIZE(0x8029).
kGL_HISTOGRAM_BLUE_SIZE(0x802A).
kGL_HISTOGRAM_ALPHA_SIZE(0x802B).
kGL_HISTOGRAM_LUMINANCE_SIZE(0x802C).
kGL_HISTOGRAM_SINK(0x802D).
kGL_MINMAX(0x802E).
kGL_MINMAX_FORMAT(0x802F).
kGL_MINMAX_SINK(0x8030).
kGL_TABLE_TOO_LARGE(0x8031).

% packed_pixels
kGL_UNSIGNED_BYTE_3_3_2(0x8032).
kGL_UNSIGNED_SHORT_4_4_4_4(0x8033).
kGL_UNSIGNED_SHORT_5_5_5_1(0x8034).
kGL_UNSIGNED_INT_8_8_8_8(0x8035).
kGL_UNSIGNED_INT_10_10_10_2(0x8036).
kGL_UNSIGNED_BYTE_2_3_3_REV(0x8362).
kGL_UNSIGNED_SHORT_5_6_5(0x8363).
kGL_UNSIGNED_SHORT_5_6_5_REV(0x8364).
kGL_UNSIGNED_SHORT_4_4_4_4_REV(0x8365).
kGL_UNSIGNED_SHORT_1_5_5_5_REV(0x8366).
kGL_UNSIGNED_INT_8_8_8_8_REV(0x8367).
kGL_UNSIGNED_INT_2_10_10_10_REV(0x8368).

% rescale_normal
kGL_RESCALE_NORMAL(0x803A).

% separate_specular_color
kGL_LIGHT_MODEL_COLOR_CONTROL(0x81F8).
kGL_SINGLE_COLOR(0x81F9).
kGL_SEPARATE_SPECULAR_COLOR(0x81FA).

% texture3D
kGL_PACK_SKIP_IMAGES(0x806B).
kGL_PACK_IMAGE_HEIGHT(0x806C).
kGL_UNPACK_SKIP_IMAGES(0x806D).
kGL_UNPACK_IMAGE_HEIGHT(0x806E).
kGL_TEXTURE_3D(0x806F).
kGL_PROXY_TEXTURE_3D(0x8070).
kGL_TEXTURE_DEPTH(0x8071).
kGL_TEXTURE_WRAP_R(0x8072).
kGL_MAX_3D_TEXTURE_SIZE(0x8073).

% texture_edge_clamp
kGL_CLAMP_TO_EDGE(0x812F).
kGL_CLAMP_TO_BORDER(0x812D).

% texture_lod
kGL_TEXTURE_MIN_LOD(0x813A).
kGL_TEXTURE_MAX_LOD(0x813B).
kGL_TEXTURE_BASE_LEVEL(0x813C).
kGL_TEXTURE_MAX_LEVEL(0x813D).

% GetTarget1_2
kGL_SMOOTH_POINT_SIZE_RANGE(0x0B12).
kGL_SMOOTH_POINT_SIZE_GRANULARITY(0x0B13).
kGL_SMOOTH_LINE_WIDTH_RANGE(0x0B22).
kGL_SMOOTH_LINE_WIDTH_GRANULARITY(0x0B23).
kGL_ALIASED_POINT_SIZE_RANGE(0x846D).
kGL_ALIASED_LINE_WIDTH_RANGE(0x846E).

kGL_TEXTURE0(0x84C0).
kGL_TEXTURE1(0x84C1).
kGL_TEXTURE2(0x84C2).
kGL_TEXTURE3(0x84C3).
kGL_TEXTURE4(0x84C4).
kGL_TEXTURE5(0x84C5).
kGL_TEXTURE6(0x84C6).
kGL_TEXTURE7(0x84C7).
kGL_TEXTURE8(0x84C8).
kGL_TEXTURE9(0x84C9).
kGL_TEXTURE10(0x84CA).
kGL_TEXTURE11(0x84CB).
kGL_TEXTURE12(0x84CC).
kGL_TEXTURE13(0x84CD).
kGL_TEXTURE14(0x84CE).
kGL_TEXTURE15(0x84CF).
kGL_TEXTURE16(0x84D0).
kGL_TEXTURE17(0x84D1).
kGL_TEXTURE18(0x84D2).
kGL_TEXTURE19(0x84D3).
kGL_TEXTURE20(0x84D4).
kGL_TEXTURE21(0x84D5).
kGL_TEXTURE22(0x84D6).
kGL_TEXTURE23(0x84D7).
kGL_TEXTURE24(0x84D8).
kGL_TEXTURE25(0x84D9).
kGL_TEXTURE26(0x84DA).
kGL_TEXTURE27(0x84DB).
kGL_TEXTURE28(0x84DC).
kGL_TEXTURE29(0x84DD).
kGL_TEXTURE30(0x84DE).
kGL_TEXTURE31(0x84DF).
kGL_ACTIVE_TEXTURE(0x84E0).
kGL_CLIENT_ACTIVE_TEXTURE(0x84E1).
kGL_MAX_TEXTURE_UNITS(0x84E2).

kGL_COMBINE(0x8570).
kGL_COMBINE_RGB(0x8571).
kGL_COMBINE_ALPHA(0x8572).
kGL_RGB_SCALE(0x8573).
kGL_ADD_SIGNED(0x8574).
kGL_INTERPOLATE(0x8575).
kGL_CONSTANT(0x8576).
kGL_PRIMARY_COLOR(0x8577).
kGL_PREVIOUS(0x8578).
kGL_SUBTRACT(0x84E7).

kGL_SRC0_RGB(0x8580).
kGL_SRC1_RGB(0x8581).
kGL_SRC2_RGB(0x8582).
kGL_SRC3_RGB(0x8583).
kGL_SRC4_RGB(0x8584).
kGL_SRC5_RGB(0x8585).
kGL_SRC6_RGB(0x8586).
kGL_SRC7_RGB(0x8587).
kGL_SRC0_ALPHA(0x8588).
kGL_SRC1_ALPHA(0x8589).
kGL_SRC2_ALPHA(0x858A).
kGL_SRC3_ALPHA(0x858B).
kGL_SRC4_ALPHA(0x858C).
kGL_SRC5_ALPHA(0x858D).
kGL_SRC6_ALPHA(0x858E).
kGL_SRC7_ALPHA(0x858F).

% occlusion_query
kGL_QUERY_COUNTER_BITS(0x8864).
kGL_CURRENT_QUERY(0x8865).
kGL_QUERY_RESULT(0x8866).
kGL_QUERY_RESULT_AVAILABLE(0x8867).
kGL_SAMPLES_PASSED(0x8914).

kGL_FOG_COORD_SRC(0x8450).
kGL_FOG_COORD(0x8451).
kGL_FRAGMENT_DEPTH(0x8452).
kGL_CURRENT_FOG_COORD(0x8453).
kGL_FOG_COORD_ARRAY_TYPE(0x8454).
kGL_FOG_COORD_ARRAY_STRIDE(0x8455).
kGL_FOG_COORD_ARRAY_POINTER(0x8456).
kGL_FOG_COORD_ARRAY(0x8457).

% vertex_buffer_object
kGL_ARRAY_BUFFER(0x8892).
kGL_ELEMENT_ARRAY_BUFFER(0x8893).
kGL_ARRAY_BUFFER_BINDING(0x8894).
kGL_ELEMENT_ARRAY_BUFFER_BINDING(0x8895).
kGL_VERTEX_ARRAY_BUFFER_BINDING(0x8896).
kGL_NORMAL_ARRAY_BUFFER_BINDING(0x8897).
kGL_COLOR_ARRAY_BUFFER_BINDING(0x8898).
kGL_INDEX_ARRAY_BUFFER_BINDING(0x8899).
kGL_TEXTURE_COORD_ARRAY_BUFFER_BINDING(0x889A).
kGL_EDGE_FLAG_ARRAY_BUFFER_BINDING(0x889B).
kGL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING(0x889C).
kGL_FOG_COORD_ARRAY_BUFFER_BINDING(0x889D).
kGL_WEIGHT_ARRAY_BUFFER_BINDING(0x889E).
kGL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING(0x889F).
kGL_STREAM_DRAW(0x88E0).
kGL_STREAM_READ(0x88E1).
kGL_STREAM_COPY(0x88E2).
kGL_STATIC_DRAW(0x88E4).
kGL_STATIC_READ(0x88E5).
kGL_STATIC_COPY(0x88E6).
kGL_DYNAMIC_DRAW(0x88E8).
kGL_DYNAMIC_READ(0x88E9).
kGL_DYNAMIC_COPY(0x88EA).
kGL_READ_ONLY(0x88B8).
kGL_WRITE_ONLY(0x88B9).
kGL_READ_WRITE(0x88BA).
kGL_BUFFER_SIZE(0x8764).
kGL_BUFFER_USAGE(0x8765).
kGL_BUFFER_ACCESS(0x88BB).
kGL_BUFFER_MAPPED(0x88BC).
kGL_BUFFER_MAP_POINTER(0x88BD).

% OpenGL 2.0
kGL_CURRENT_PROGRAM(0x8B8D).
kGL_SHADER_TYPE(0x8B4F).
kGL_DELETE_STATUS(0x8B80).
kGL_COMPILE_STATUS(0x8B81).
kGL_LINK_STATUS(0x8B82).
kGL_VALIDATE_STATUS(0x8B83).
kGL_INFO_LOG_LENGTH(0x8B84).
kGL_ATTACHED_SHADERS(0x8B85).
kGL_ACTIVE_UNIFORMS(0x8B86).
kGL_ACTIVE_UNIFORM_MAX_LENGTH(0x8B87).
kGL_SHADER_SOURCE_LENGTH(0x8B88).
kGL_FLOAT_VEC2(0x8B50).
kGL_FLOAT_VEC3(0x8B51).
kGL_FLOAT_VEC4(0x8B52).
kGL_INT_VEC2(0x8B53).
kGL_INT_VEC3(0x8B54).
kGL_INT_VEC4(0x8B55).
kGL_BOOL(0x8B56).
kGL_BOOL_VEC2(0x8B57).
kGL_BOOL_VEC3(0x8B58).
kGL_BOOL_VEC4(0x8B59).
kGL_FLOAT_MAT2(0x8B5A).
kGL_FLOAT_MAT3(0x8B5B).
kGL_FLOAT_MAT4(0x8B5C).
kGL_SAMPLER_1D(0x8B5D).
kGL_SAMPLER_2D(0x8B5E).
kGL_SAMPLER_3D(0x8B5F).
kGL_SAMPLER_CUBE(0x8B60).
kGL_SAMPLER_1D_SHADOW(0x8B61).
kGL_SAMPLER_2D_SHADOW(0x8B62).
kGL_SHADING_LANGUAGE_VERSION(0x8B8C).
kGL_VERTEX_SHADER(0x8B31).
kGL_MAX_VERTEX_UNIFORM_COMPONENTS(0x8B4A).
kGL_MAX_VARYING_FLOATS(0x8B4B).
kGL_MAX_VERTEX_TEXTURE_IMAGE_UNITS(0x8B4C).
kGL_MAX_COMBINED_TEXTURE_IMAGE_UNITS(0x8B4D).
kGL_ACTIVE_ATTRIBUTES(0x8B89).
kGL_ACTIVE_ATTRIBUTE_MAX_LENGTH(0x8B8A).
kGL_FRAGMENT_SHADER(0x8B30).
kGL_MAX_FRAGMENT_UNIFORM_COMPONENTS(0x8B49).
kGL_FRAGMENT_SHADER_DERIVATIVE_HINT(0x8B8B).
kGL_MAX_VERTEX_ATTRIBS(0x8869).
kGL_VERTEX_ATTRIB_ARRAY_ENABLED(0x8622).
kGL_VERTEX_ATTRIB_ARRAY_SIZE(0x8623).
kGL_VERTEX_ATTRIB_ARRAY_STRIDE(0x8624).
kGL_VERTEX_ATTRIB_ARRAY_TYPE(0x8625).
kGL_VERTEX_ATTRIB_ARRAY_NORMALIZED(0x886A).
kGL_CURRENT_VERTEX_ATTRIB(0x8626).
kGL_VERTEX_ATTRIB_ARRAY_POINTER(0x8645).
kGL_VERTEX_PROGRAM_POINT_SIZE(0x8642).
kGL_VERTEX_PROGRAM_TWO_SIDE(0x8643).
kGL_MAX_TEXTURE_COORDS(0x8871).
kGL_MAX_TEXTURE_IMAGE_UNITS(0x8872).
kGL_MAX_DRAW_BUFFERS(0x8824).
kGL_DRAW_BUFFER0(0x8825).
kGL_DRAW_BUFFER1(0x8826).
kGL_DRAW_BUFFER2(0x8827).
kGL_DRAW_BUFFER3(0x8828).
kGL_DRAW_BUFFER4(0x8829).
kGL_DRAW_BUFFER5(0x882A).
kGL_DRAW_BUFFER6(0x882B).
kGL_DRAW_BUFFER7(0x882C).
kGL_DRAW_BUFFER8(0x882D).
kGL_DRAW_BUFFER9(0x882E).
kGL_DRAW_BUFFER10(0x882F).
kGL_DRAW_BUFFER11(0x8830).
kGL_DRAW_BUFFER12(0x8831).
kGL_DRAW_BUFFER13(0x8832).
kGL_DRAW_BUFFER14(0x8833).
kGL_DRAW_BUFFER15(0x8834).
kGL_POINT_SPRITE(0x8861).
kGL_COORD_REPLACE(0x8862).
kGL_POINT_SPRITE_COORD_ORIGIN(0x8CA0).
kGL_LOWER_LEFT(0x8CA1).
kGL_UPPER_LEFT(0x8CA2).
kGL_STENCIL_BACK_FUNC(0x8800).
kGL_STENCIL_BACK_VALUE_MASK(0x8CA4).
kGL_STENCIL_BACK_REF(0x8CA3).
kGL_STENCIL_BACK_FAIL(0x8801).
kGL_STENCIL_BACK_PASS_DEPTH_FAIL(0x8802).
kGL_STENCIL_BACK_PASS_DEPTH_PASS(0x8803).
kGL_STENCIL_BACK_WRITEMASK(0x8CA5).

% OpenGL 2.1
kGL_CURRENT_RASTER_SECONDARY_COLOR(0x845F).
kGL_PIXEL_PACK_BUFFER(0x88EB).
kGL_PIXEL_UNPACK_BUFFER(0x88EC).
kGL_PIXEL_PACK_BUFFER_BINDING(0x88ED).
kGL_PIXEL_UNPACK_BUFFER_BINDING(0x88EF).
kGL_FLOAT_MAT2x3(0x8B65).
kGL_FLOAT_MAT2x4(0x8B66).
kGL_FLOAT_MAT3x2(0x8B67).
kGL_FLOAT_MAT3x4(0x8B68).
kGL_FLOAT_MAT4x2(0x8B69).
kGL_FLOAT_MAT4x3(0x8B6A).
kGL_SRGB(0x8C40).
kGL_SRGB8(0x8C41).
kGL_SRGB_ALPHA(0x8C42).
kGL_SRGB8_ALPHA8(0x8C43).
kGL_SLUMINANCE_ALPHA(0x8C44).
kGL_SLUMINANCE8_ALPHA8(0x8C45).
kGL_SLUMINANCE(0x8C46).
kGL_SLUMINANCE8(0x8C47).
kGL_COMPRESSED_SRGB(0x8C48).
kGL_COMPRESSED_SRGB_ALPHA(0x8C49).
kGL_COMPRESSED_SLUMINANCE(0x8C4A).
kGL_COMPRESSED_SLUMINANCE_ALPHA(0x8C4B).
