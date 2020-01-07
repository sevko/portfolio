# Seam Carving
Simple seam-carving. Loads "./out.bmp", and anytime you press Enter it carves out a vertical seam. Only the energies
of pixels adjacent to the previously removed seam are re-computed for the next removal, since these are the only ones
that will have changed. `./seam-carver -s` to draw seams as they're removed.
