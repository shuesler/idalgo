uploader <- function(media, path, name) {

  googledrive::drive_upload(
    media,
    path,
    name,
    type = "image/jpeg"
  ) %>% googledrive::drive_share(role = "reader", type = "anyone")

}
