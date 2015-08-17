ecm_add_app_icon(SNORENOTIFY_SNORESEND_DEPS ICONS ${PROJECT_SOURCE_DIR}/data/128-apps-snore.png)

add_executable(snoresend main.cpp ${SNORENOTIFY_SNORESEND_DEPS})
target_link_libraries(snoresend Snore::Libsnore)
ecm_mark_nongui_executable(snoresend)

install(TARGETS snoresend ${KDE_INSTALL_TARGETS_DEFAULT_ARGS})
