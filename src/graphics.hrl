

-define(GRAPHICS_DEFAULT_NUM_SEGMENTS, 16).

% protocol message passing definitions
-define(GRAPHICS_SET_COLOR_MESSAGE      , graphics_set_color).
-define(GRAPHICS_GET_COLOR_MESSAGE      , graphics_get_color).

-define(GRAPHICS_SET_CLEAR_COLOR_MESSAGE, graphics_set_clear_color).
-define(GRAPHICS_GET_CLEAR_COLOR_MESSAGE, graphics_get_clear_color).

-define(GRAPHICS_GET_SIZE_MESSAGE       , graphics_get_size).

-define(GRAPHICS_CLEAR_MESSAGE          , graphics_clear).
-define(GRAPHICS_FILL_MESSAGE           , graphics_fill).

-define(GRAPHICS_DRAW_RECT_MESSAGE      , graphics_draw_rect).
-define(GRAPHICS_FILL_RECT_MESSAGE      , graphics_fill_rect).
-define(GRAPHICS_DRAW_LINE_MESSAGE      , graphics_draw_line).
-define(GRAPHICS_DRAW_OVAL_MESSAGE      , graphics_draw_oval).
-define(GRAPHICS_FILL_OVAL_MESSAGE      , graphics_fill_oval).
-define(GRAPHICS_DRAW_POINT_MESSAGE     , graphics_draw_point).
-define(GRAPHICS_DRAW_POLYGON_MESSAGE   , graphics_draw_polygon).
-define(GRAPHICS_FILL_POLYGON_MESSAGE   , graphics_fill_polygon).
-define(GRAPHICS_DRAW_IMAGE_MESSAGE     , graphics_draw_image).

-define(GRAPHICS_NEW_IMAGE              , graphics_new_image).
-define(GRAPHICS_DESTROY_IMAGE          , graphics_destroy_image).

-define(GRAPHICS_PAINT_ACTOR            , graphics_paint_actor).
