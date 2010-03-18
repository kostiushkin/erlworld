
-define(IMAGE_NEW_IMAGE     , image_new_image     ).
-define(IMAGE_DESTROY_IMAGE , image_destroy_image ).

-record(img, {
       width,       % the height of this image, in pixels.
       height,      % the width of this image, in pixels.
       drawWidth,   % the UV map width position for when this is draw and mapped to a quad
       drawHeight,  % the UV map height position for when this is mapped to a quad
       glTextureID, % the internal OpenGL texture number
       destroyer    % whoever the image should contact in regards to being destroyed
}).