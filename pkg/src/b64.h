/*
 * b64.h, a subset of the markdown package's Rmarkdown.h
 * 
 * Copyright (C) 2009-1012 by RStudio, Inc.
 * 
 * This program is licensed to you under the terms of version 3 of the
 * GNU General Public License. This program is distributed WITHOUT ANY
 * EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
 * GPL (http://www.gnu.org/licenses/gpl-3.0.txt) for more details.
 *
 */

#include <R.h>
#include <Rinternals.h>
#include "buffer.h"
#define READ_UNIT 1024
#define OUTPUT_UNIT 64
#define B64_WARNING_NOMEM warning("Out of memory!")

extern SEXP b64encode(SEXP Sdata);
