/**************************************************************************/
/*                                                                        */
/*   Typerex Tools                                                        */
/*                                                                        */
/*   Copyright 2011-2017 OCamlPro SAS                                     */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU General Public License version 3 described in the file       */
/*   LICENSE.                                                             */
/*                                                                        */
/**************************************************************************/

#ifndef __GPROF_H__
#define __GPROF_H__

/* caml_gcprof_flag:
    | 1 => gcprof active
    | 2 => LOGGING mode
    | 4 => COUNTERS mode (flushed at end, and at every watermark)
    | 8 => COUNTERS mode flushed at every GC

    | 16 => counters (LOGGING and COUNTERS) for minor GC too
              (much more expensive).
    | 32 => counters (LOGGING _or_ COUNTERS) for threads.
*/

CAMLextern uintnat ocp_gcprof_profile_period;
CAMLextern int ocp_gcprof_profile_major;

CAMLextern void ocp_gcprof_alloc_major(mlsize_t sz, tag_t tag,
                                       uintnat locid);
CAMLextern void ocp_gcprof_minor_collection();
CAMLextern void ocp_gcprof_future_minor_alloc(char* young_ptr, 
                                              mlsize_t size_plus_header);


#endif // __GPROF_H__
