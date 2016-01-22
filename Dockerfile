FROM epiviz/epivizr_base

COPY . /epivizr
RUN installPackage.r -p /epivizr
