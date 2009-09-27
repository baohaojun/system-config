# Microsoft Developer Studio Project File - Name="RunBhjRun" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=RunBhjRun - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "RunBhjRun.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "RunBhjRun.mak" CFG="RunBhjRun - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "RunBhjRun - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "RunBhjRun - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "RunBhjRun - Win32 Release"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_AFXDLL" /c
# ADD CPP /nologo /MD /W3 /GX /Od /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" /D "BOOST_REGEX_DYN_LINK" /Zm800 /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x804 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x804 /d "NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /machine:I386
# ADD LINK32 /nologo /subsystem:windows /pdb:none /machine:I386

!ELSEIF  "$(CFG)" == "RunBhjRun - Win32 Debug"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D /GZ "_AFXDLL" /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" /D _STLP_DEBUG=1 /D /GZ /Zm800 /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x804 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x804 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 /nologo /subsystem:windows /pdb:none /debug /machine:I386

!ENDIF 

# Begin Target

# Name "RunBhjRun - Win32 Release"
# Name "RunBhjRun - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\bhjlib.cpp
DEP_CPP_BHJLI=\
	".\bhjlib.h"\
	
NODEP_CPP_BHJLI=\
	"..\..\usr\include\pthread.h"\
	"..\..\usr\include\sys\cdefs.h"\
	".\em.h"\
	".\oost\assert.hpp"\
	".\oost\checked_delete.hpp"\
	".\oost\config.hpp"\
	".\oost\config\auto_link.hpp"\
	".\oost\config\no_tr1\utility.hpp"\
	".\oost\config\posix_features.hpp"\
	".\oost\config\requires_threads.hpp"\
	".\oost\config\select_compiler_config.hpp"\
	".\oost\config\select_platform_config.hpp"\
	".\oost\config\select_stdlib_config.hpp"\
	".\oost\config\suffix.hpp"\
	".\oost\cstdint.hpp"\
	".\oost\current_function.hpp"\
	".\oost\detail\atomic_count.hpp"\
	".\oost\detail\atomic_count_gcc.hpp"\
	".\oost\detail\atomic_count_pthreads.hpp"\
	".\oost\detail\atomic_count_win32.hpp"\
	".\oost\detail\bad_weak_ptr.hpp"\
	".\oost\detail\endian.hpp"\
	".\oost\detail\interlocked.hpp"\
	".\oost\detail\lightweight_mutex.hpp"\
	".\oost\detail\limits.hpp"\
	".\oost\detail\lwm_nop.hpp"\
	".\oost\detail\lwm_pthreads.hpp"\
	".\oost\detail\lwm_win32_cs.hpp"\
	".\oost\detail\quick_allocator.hpp"\
	".\oost\detail\shared_count.hpp"\
	".\oost\detail\shared_ptr_nmt.hpp"\
	".\oost\detail\sp_counted_base.hpp"\
	".\oost\detail\sp_counted_base_cw_ppc.hpp"\
	".\oost\detail\sp_counted_base_gcc_ia64.hpp"\
	".\oost\detail\sp_counted_base_gcc_ppc.hpp"\
	".\oost\detail\sp_counted_base_gcc_x86.hpp"\
	".\oost\detail\sp_counted_base_nt.hpp"\
	".\oost\detail\sp_counted_base_pt.hpp"\
	".\oost\detail\sp_counted_base_w32.hpp"\
	".\oost\detail\sp_counted_impl.hpp"\
	".\oost\detail\workaround.hpp"\
	".\oost\limits.hpp"\
	".\oost\mpl\aux_\adl_barrier.hpp"\
	".\oost\mpl\aux_\arity.hpp"\
	".\oost\mpl\aux_\config\adl.hpp"\
	".\oost\mpl\aux_\config\arrays.hpp"\
	".\oost\mpl\aux_\config\ctps.hpp"\
	".\oost\mpl\aux_\config\dtp.hpp"\
	".\oost\mpl\aux_\config\eti.hpp"\
	".\oost\mpl\aux_\config\gcc.hpp"\
	".\oost\mpl\aux_\config\has_xxx.hpp"\
	".\oost\mpl\aux_\config\integral.hpp"\
	".\oost\mpl\aux_\config\intel.hpp"\
	".\oost\mpl\aux_\config\lambda.hpp"\
	".\oost\mpl\aux_\config\msvc.hpp"\
	".\oost\mpl\aux_\config\msvc_typename.hpp"\
	".\oost\mpl\aux_\config\nttp.hpp"\
	".\oost\mpl\aux_\config\overload_resolution.hpp"\
	".\oost\mpl\aux_\config\preprocessor.hpp"\
	".\oost\mpl\aux_\config\static_constant.hpp"\
	".\oost\mpl\aux_\config\ttp.hpp"\
	".\oost\mpl\aux_\config\workaround.hpp"\
	".\oost\mpl\aux_\integral_wrapper.hpp"\
	".\oost\mpl\aux_\lambda_arity_param.hpp"\
	".\oost\mpl\aux_\lambda_support.hpp"\
	".\oost\mpl\aux_\na.hpp"\
	".\oost\mpl\aux_\na_fwd.hpp"\
	".\oost\mpl\aux_\na_spec.hpp"\
	".\oost\mpl\aux_\nttp_decl.hpp"\
	".\oost\mpl\aux_\preprocessor\def_params_tail.hpp"\
	".\oost\mpl\aux_\preprocessor\enum.hpp"\
	".\oost\mpl\aux_\preprocessor\filter_params.hpp"\
	".\oost\mpl\aux_\preprocessor\params.hpp"\
	".\oost\mpl\aux_\preprocessor\sub.hpp"\
	".\oost\mpl\aux_\preprocessor\tuple.hpp"\
	".\oost\mpl\aux_\static_cast.hpp"\
	".\oost\mpl\aux_\template_arity_fwd.hpp"\
	".\oost\mpl\aux_\type_wrapper.hpp"\
	".\oost\mpl\aux_\value_wknd.hpp"\
	".\oost\mpl\aux_\yes_no.hpp"\
	".\oost\mpl\bool.hpp"\
	".\oost\mpl\bool_fwd.hpp"\
	".\oost\mpl\has_xxx.hpp"\
	".\oost\mpl\if.hpp"\
	".\oost\mpl\int.hpp"\
	".\oost\mpl\int_fwd.hpp"\
	".\oost\mpl\integral_c.hpp"\
	".\oost\mpl\integral_c_fwd.hpp"\
	".\oost\mpl\integral_c_tag.hpp"\
	".\oost\mpl\lambda_fwd.hpp"\
	".\oost\mpl\limits\arity.hpp"\
	".\oost\mpl\size_t.hpp"\
	".\oost\mpl\size_t_fwd.hpp"\
	".\oost\mpl\void_fwd.hpp"\
	".\oost\next_prior.hpp"\
	".\oost\non_type.hpp"\
	".\oost\noncopyable.hpp"\
	".\oost\preprocessor\arithmetic\add.hpp"\
	".\oost\preprocessor\arithmetic\inc.hpp"\
	".\oost\preprocessor\arithmetic\sub.hpp"\
	".\oost\preprocessor\cat.hpp"\
	".\oost\preprocessor\comma_if.hpp"\
	".\oost\preprocessor\config\config.hpp"\
	".\oost\preprocessor\control\detail\dmc\while.hpp"\
	".\oost\preprocessor\control\detail\edg\while.hpp"\
	".\oost\preprocessor\control\detail\msvc\while.hpp"\
	".\oost\preprocessor\control\detail\while.hpp"\
	".\oost\preprocessor\control\if.hpp"\
	".\oost\preprocessor\control\iif.hpp"\
	".\oost\preprocessor\control\while.hpp"\
	".\oost\preprocessor\debug\error.hpp"\
	".\oost\preprocessor\detail\auto_rec.hpp"\
	".\oost\preprocessor\detail\check.hpp"\
	".\oost\preprocessor\detail\dmc\auto_rec.hpp"\
	".\oost\preprocessor\detail\is_binary.hpp"\
	".\oost\preprocessor\empty.hpp"\
	".\oost\preprocessor\enum_params.hpp"\
	".\oost\preprocessor\facilities\empty.hpp"\
	".\oost\preprocessor\identity.hpp"\
	".\oost\preprocessor\inc.hpp"\
	".\oost\preprocessor\iterate.hpp"\
	".\oost\preprocessor\list\adt.hpp"\
	".\oost\preprocessor\list\append.hpp"\
	".\oost\preprocessor\list\detail\dmc\fold_left.hpp"\
	".\oost\preprocessor\list\detail\edg\fold_left.hpp"\
	".\oost\preprocessor\list\detail\edg\fold_right.hpp"\
	".\oost\preprocessor\list\detail\fold_left.hpp"\
	".\oost\preprocessor\list\detail\fold_right.hpp"\
	".\oost\preprocessor\list\fold_left.hpp"\
	".\oost\preprocessor\list\fold_right.hpp"\
	".\oost\preprocessor\list\for_each_i.hpp"\
	".\oost\preprocessor\list\reverse.hpp"\
	".\oost\preprocessor\list\transform.hpp"\
	".\oost\preprocessor\logical\and.hpp"\
	".\oost\preprocessor\logical\bitand.hpp"\
	".\oost\preprocessor\logical\bool.hpp"\
	".\oost\preprocessor\logical\compl.hpp"\
	".\oost\preprocessor\punctuation\comma.hpp"\
	".\oost\preprocessor\punctuation\comma_if.hpp"\
	".\oost\preprocessor\repeat.hpp"\
	".\oost\preprocessor\repetition\detail\dmc\for.hpp"\
	".\oost\preprocessor\repetition\detail\edg\for.hpp"\
	".\oost\preprocessor\repetition\detail\for.hpp"\
	".\oost\preprocessor\repetition\detail\msvc\for.hpp"\
	".\oost\preprocessor\repetition\enum_binary_params.hpp"\
	".\oost\preprocessor\repetition\enum_params.hpp"\
	".\oost\preprocessor\repetition\for.hpp"\
	".\oost\preprocessor\repetition\repeat.hpp"\
	".\oost\preprocessor\repetition\repeat_from_to.hpp"\
	".\oost\preprocessor\tuple\eat.hpp"\
	".\oost\preprocessor\tuple\elem.hpp"\
	".\oost\preprocessor\tuple\rem.hpp"\
	".\oost\preprocessor\tuple\to_list.hpp"\
	".\oost\regex.hpp"\
	".\oost\regex\config.hpp"\
	".\oost\regex\config\borland.hpp"\
	".\oost\regex\config\cwchar.hpp"\
	".\oost\regex\pattern_except.hpp"\
	".\oost\regex\pending\object_cache.hpp"\
	".\oost\regex\pending\static_mutex.hpp"\
	".\oost\regex\regex_traits.hpp"\
	".\oost\regex\v4\basic_regex.hpp"\
	".\oost\regex\v4\basic_regex_creator.hpp"\
	".\oost\regex\v4\basic_regex_parser.hpp"\
	".\oost\regex\v4\c_regex_traits.hpp"\
	".\oost\regex\v4\char_regex_traits.hpp"\
	".\oost\regex\v4\cpp_regex_traits.hpp"\
	".\oost\regex\v4\error_type.hpp"\
	".\oost\regex\v4\instances.hpp"\
	".\oost\regex\v4\iterator_category.hpp"\
	".\oost\regex\v4\iterator_traits.hpp"\
	".\oost\regex\v4\match_flags.hpp"\
	".\oost\regex\v4\match_results.hpp"\
	".\oost\regex\v4\perl_matcher.hpp"\
	".\oost\regex\v4\perl_matcher_common.hpp"\
	".\oost\regex\v4\perl_matcher_non_recursive.hpp"\
	".\oost\regex\v4\perl_matcher_recursive.hpp"\
	".\oost\regex\v4\primary_transform.hpp"\
	".\oost\regex\v4\protected_call.hpp"\
	".\oost\regex\v4\regbase.hpp"\
	".\oost\regex\v4\regex.hpp"\
	".\oost\regex\v4\regex_format.hpp"\
	".\oost\regex\v4\regex_fwd.hpp"\
	".\oost\regex\v4\regex_grep.hpp"\
	".\oost\regex\v4\regex_iterator.hpp"\
	".\oost\regex\v4\regex_match.hpp"\
	".\oost\regex\v4\regex_merge.hpp"\
	".\oost\regex\v4\regex_raw_buffer.hpp"\
	".\oost\regex\v4\regex_replace.hpp"\
	".\oost\regex\v4\regex_search.hpp"\
	".\oost\regex\v4\regex_split.hpp"\
	".\oost\regex\v4\regex_token_iterator.hpp"\
	".\oost\regex\v4\regex_traits.hpp"\
	".\oost\regex\v4\regex_traits_defaults.hpp"\
	".\oost\regex\v4\regex_workaround.hpp"\
	".\oost\regex\v4\states.hpp"\
	".\oost\regex\v4\sub_match.hpp"\
	".\oost\regex\v4\syntax_type.hpp"\
	".\oost\regex\v4\w32_regex_traits.hpp"\
	".\oost\regex_fwd.hpp"\
	".\oost\scoped_array.hpp"\
	".\oost\scoped_ptr.hpp"\
	".\oost\shared_ptr.hpp"\
	".\oost\static_assert.hpp"\
	".\oost\thread\detail\config.hpp"\
	".\oost\thread\detail\lock.hpp"\
	".\oost\thread\exceptions.hpp"\
	".\oost\thread\once.hpp"\
	".\oost\thread\recursive_mutex.hpp"\
	".\oost\throw_exception.hpp"\
	".\oost\type.hpp"\
	".\oost\type_traits\add_const.hpp"\
	".\oost\type_traits\add_reference.hpp"\
	".\oost\type_traits\alignment_of.hpp"\
	".\oost\type_traits\broken_compiler_spec.hpp"\
	".\oost\type_traits\config.hpp"\
	".\oost\type_traits\detail\bool_trait_def.hpp"\
	".\oost\type_traits\detail\bool_trait_undef.hpp"\
	".\oost\type_traits\detail\cv_traits_impl.hpp"\
	".\oost\type_traits\detail\false_result.hpp"\
	".\oost\type_traits\detail\ice_and.hpp"\
	".\oost\type_traits\detail\ice_eq.hpp"\
	".\oost\type_traits\detail\ice_not.hpp"\
	".\oost\type_traits\detail\ice_or.hpp"\
	".\oost\type_traits\detail\is_function_ptr_helper.hpp"\
	".\oost\type_traits\detail\is_function_ptr_tester.hpp"\
	".\oost\type_traits\detail\is_mem_fun_pointer_impl.hpp"\
	".\oost\type_traits\detail\is_mem_fun_pointer_tester.hpp"\
	".\oost\type_traits\detail\size_t_trait_def.hpp"\
	".\oost\type_traits\detail\size_t_trait_undef.hpp"\
	".\oost\type_traits\detail\template_arity_spec.hpp"\
	".\oost\type_traits\detail\type_trait_def.hpp"\
	".\oost\type_traits\detail\type_trait_undef.hpp"\
	".\oost\type_traits\detail\wrap.hpp"\
	".\oost\type_traits\detail\yes_no_type.hpp"\
	".\oost\type_traits\has_trivial_assign.hpp"\
	".\oost\type_traits\has_trivial_constructor.hpp"\
	".\oost\type_traits\has_trivial_copy.hpp"\
	".\oost\type_traits\has_trivial_destructor.hpp"\
	".\oost\type_traits\ice.hpp"\
	".\oost\type_traits\integral_constant.hpp"\
	".\oost\type_traits\intrinsics.hpp"\
	".\oost\type_traits\is_abstract.hpp"\
	".\oost\type_traits\is_arithmetic.hpp"\
	".\oost\type_traits\is_array.hpp"\
	".\oost\type_traits\is_class.hpp"\
	".\oost\type_traits\is_const.hpp"\
	".\oost\type_traits\is_convertible.hpp"\
	".\oost\type_traits\is_enum.hpp"\
	".\oost\type_traits\is_float.hpp"\
	".\oost\type_traits\is_function.hpp"\
	".\oost\type_traits\is_integral.hpp"\
	".\oost\type_traits\is_member_function_pointer.hpp"\
	".\oost\type_traits\is_member_pointer.hpp"\
	".\oost\type_traits\is_pod.hpp"\
	".\oost\type_traits\is_pointer.hpp"\
	".\oost\type_traits\is_polymorphic.hpp"\
	".\oost\type_traits\is_reference.hpp"\
	".\oost\type_traits\is_same.hpp"\
	".\oost\type_traits\is_scalar.hpp"\
	".\oost\type_traits\is_union.hpp"\
	".\oost\type_traits\is_void.hpp"\
	".\oost\type_traits\is_volatile.hpp"\
	".\oost\type_traits\msvc\remove_bounds.hpp"\
	".\oost\type_traits\msvc\remove_cv.hpp"\
	".\oost\type_traits\msvc\remove_reference.hpp"\
	".\oost\type_traits\msvc\typeof.hpp"\
	".\oost\type_traits\remove_bounds.hpp"\
	".\oost\type_traits\remove_cv.hpp"\
	".\oost\type_traits\remove_reference.hpp"\
	".\oost\type_traits\type_with_alignment.hpp"\
	".\oost\utility.hpp"\
	".\oost\utility\addressof.hpp"\
	".\oost\utility\base_from_member.hpp"\
	".\oost\utility\enable_if.hpp"\
	".\oost\version.hpp"\
	".\sing\cstring"\
	".\thread.h"\
	".\tl\_abbrevs.h"\
	".\tl\_algobase.c"\
	".\tl\_algobase.h"\
	".\tl\_alloc.c"\
	".\tl\_alloc.h"\
	".\tl\_cmath.h"\
	".\tl\_config_compat_post.h"\
	".\tl\_construct.h"\
	".\tl\_cprolog.h"\
	".\tl\_cstdarg.h"\
	".\tl\_cstddef.h"\
	".\tl\_cstdio.h"\
	".\tl\_cstdlib.h"\
	".\tl\_cstring.h"\
	".\tl\_ctime.h"\
	".\tl\_ctraits_fns.h"\
	".\tl\_cwchar.h"\
	".\tl\_epilog.h"\
	".\tl\_exception.h"\
	".\tl\_function.h"\
	".\tl\_function_adaptors.h"\
	".\tl\_function_base.h"\
	".\tl\_iosfwd.h"\
	".\tl\_iterator.h"\
	".\tl\_iterator_base.h"\
	".\tl\_iterator_old.h"\
	".\tl\_mbstate_t.h"\
	".\tl\_move_construct_fwk.h"\
	".\tl\_new.h"\
	".\tl\_pair.h"\
	".\tl\_prolog.h"\
	".\tl\_pthread_alloc.h"\
	".\tl\_ptrs_specialize.h"\
	".\tl\_range_errors.c"\
	".\tl\_range_errors.h"\
	".\tl\_sparc_atomic.h"\
	".\tl\_stdexcept.h"\
	".\tl\_stdexcept_base.c"\
	".\tl\_stdexcept_base.h"\
	".\tl\_stlport_version.h"\
	".\tl\_string.c"\
	".\tl\_string.h"\
	".\tl\_string_base.h"\
	".\tl\_string_fwd.h"\
	".\tl\_string_npos.h"\
	".\tl\_string_operators.h"\
	".\tl\_string_sum.h"\
	".\tl\_string_sum_methods.h"\
	".\tl\_string_workaround.h"\
	".\tl\_threads.c"\
	".\tl\_threads.h"\
	".\tl\_uninitialized.h"\
	".\tl\boost_type_traits.h"\
	".\tl\char_traits.h"\
	".\tl\config\_aix.h"\
	".\tl\config\_apcc.h"\
	".\tl\config\_as400.h"\
	".\tl\config\_auto_link.h"\
	".\tl\config\_bc.h"\
	".\tl\config\_como.h"\
	".\tl\config\_cygwin.h"\
	".\tl\config\_dec.h"\
	".\tl\config\_dec_vms.h"\
	".\tl\config\_detect_dll_or_lib.h"\
	".\tl\config\_dm.h"\
	".\tl\config\_epilog.h"\
	".\tl\config\_evc.h"\
	".\tl\config\_feedback.h"\
	".\tl\config\_freebsd.h"\
	".\tl\config\_fujitsu.h"\
	".\tl\config\_gcc.h"\
	".\tl\config\_hpacc.h"\
	".\tl\config\_hpux.h"\
	".\tl\config\_ibm.h"\
	".\tl\config\_icc.h"\
	".\tl\config\_intel.h"\
	".\tl\config\_kai.h"\
	".\tl\config\_linux.h"\
	".\tl\config\_mac.h"\
	".\tl\config\_macosx.h"\
	".\tl\config\_msvc.h"\
	".\tl\config\_mwerks.h"\
	".\tl\config\_native_headers.h"\
	".\tl\config\_openbsd.h"\
	".\tl\config\_prolog.h"\
	".\tl\config\_sgi.h"\
	".\tl\config\_solaris.h"\
	".\tl\config\_sunprocc.h"\
	".\tl\config\_system.h"\
	".\tl\config\_warnings_off.h"\
	".\tl\config\_watcom.h"\
	".\tl\config\_windows.h"\
	".\tl\config\compat.h"\
	".\tl\config\features.h"\
	".\tl\config\host.h"\
	".\tl\config\stl_confix.h"\
	".\tl\config\stl_mycomp.h"\
	".\tl\config\user_config.h"\
	".\tl\debug\_debug.c"\
	".\tl\debug\_debug.h"\
	".\tl\debug\_iterator.h"\
	".\tl\debug\_string.h"\
	".\tl\debug\_string_sum_methods.h"\
	".\tl\msl_string.h"\
	".\tl\type_manips.h"\
	".\tl\type_traits.h"\
	"D:\bhj\boost_1_34_1\boost\thread\scoped_critical_region.hpp"\
	
# End Source File
# Begin Source File

SOURCE=.\EkbEdit.cpp
DEP_CPP_EKBED=\
	".\bhjlib.h"\
	".\EkbEdit.h"\
	".\RunBhjRun.h"\
	".\simplewnd.h"\
	".\StdAfx.h"\
	
NODEP_CPP_EKBED=\
	"..\..\usr\include\pthread.h"\
	"..\..\usr\include\sys\cdefs.h"\
	".\em.h"\
	".\hjdebug.h"\
	".\oost\assert.hpp"\
	".\oost\checked_delete.hpp"\
	".\oost\config.hpp"\
	".\oost\config\auto_link.hpp"\
	".\oost\config\no_tr1\utility.hpp"\
	".\oost\config\posix_features.hpp"\
	".\oost\config\requires_threads.hpp"\
	".\oost\config\select_compiler_config.hpp"\
	".\oost\config\select_platform_config.hpp"\
	".\oost\config\select_stdlib_config.hpp"\
	".\oost\config\suffix.hpp"\
	".\oost\cstdint.hpp"\
	".\oost\current_function.hpp"\
	".\oost\detail\atomic_count.hpp"\
	".\oost\detail\atomic_count_gcc.hpp"\
	".\oost\detail\atomic_count_pthreads.hpp"\
	".\oost\detail\atomic_count_win32.hpp"\
	".\oost\detail\bad_weak_ptr.hpp"\
	".\oost\detail\endian.hpp"\
	".\oost\detail\interlocked.hpp"\
	".\oost\detail\lightweight_mutex.hpp"\
	".\oost\detail\limits.hpp"\
	".\oost\detail\lwm_nop.hpp"\
	".\oost\detail\lwm_pthreads.hpp"\
	".\oost\detail\lwm_win32_cs.hpp"\
	".\oost\detail\quick_allocator.hpp"\
	".\oost\detail\shared_count.hpp"\
	".\oost\detail\shared_ptr_nmt.hpp"\
	".\oost\detail\sp_counted_base.hpp"\
	".\oost\detail\sp_counted_base_cw_ppc.hpp"\
	".\oost\detail\sp_counted_base_gcc_ia64.hpp"\
	".\oost\detail\sp_counted_base_gcc_ppc.hpp"\
	".\oost\detail\sp_counted_base_gcc_x86.hpp"\
	".\oost\detail\sp_counted_base_nt.hpp"\
	".\oost\detail\sp_counted_base_pt.hpp"\
	".\oost\detail\sp_counted_base_w32.hpp"\
	".\oost\detail\sp_counted_impl.hpp"\
	".\oost\detail\workaround.hpp"\
	".\oost\limits.hpp"\
	".\oost\mpl\aux_\adl_barrier.hpp"\
	".\oost\mpl\aux_\arity.hpp"\
	".\oost\mpl\aux_\config\adl.hpp"\
	".\oost\mpl\aux_\config\arrays.hpp"\
	".\oost\mpl\aux_\config\ctps.hpp"\
	".\oost\mpl\aux_\config\dtp.hpp"\
	".\oost\mpl\aux_\config\eti.hpp"\
	".\oost\mpl\aux_\config\gcc.hpp"\
	".\oost\mpl\aux_\config\has_xxx.hpp"\
	".\oost\mpl\aux_\config\integral.hpp"\
	".\oost\mpl\aux_\config\intel.hpp"\
	".\oost\mpl\aux_\config\lambda.hpp"\
	".\oost\mpl\aux_\config\msvc.hpp"\
	".\oost\mpl\aux_\config\msvc_typename.hpp"\
	".\oost\mpl\aux_\config\nttp.hpp"\
	".\oost\mpl\aux_\config\overload_resolution.hpp"\
	".\oost\mpl\aux_\config\preprocessor.hpp"\
	".\oost\mpl\aux_\config\static_constant.hpp"\
	".\oost\mpl\aux_\config\ttp.hpp"\
	".\oost\mpl\aux_\config\workaround.hpp"\
	".\oost\mpl\aux_\integral_wrapper.hpp"\
	".\oost\mpl\aux_\lambda_arity_param.hpp"\
	".\oost\mpl\aux_\lambda_support.hpp"\
	".\oost\mpl\aux_\na.hpp"\
	".\oost\mpl\aux_\na_fwd.hpp"\
	".\oost\mpl\aux_\na_spec.hpp"\
	".\oost\mpl\aux_\nttp_decl.hpp"\
	".\oost\mpl\aux_\preprocessor\def_params_tail.hpp"\
	".\oost\mpl\aux_\preprocessor\enum.hpp"\
	".\oost\mpl\aux_\preprocessor\filter_params.hpp"\
	".\oost\mpl\aux_\preprocessor\params.hpp"\
	".\oost\mpl\aux_\preprocessor\sub.hpp"\
	".\oost\mpl\aux_\preprocessor\tuple.hpp"\
	".\oost\mpl\aux_\static_cast.hpp"\
	".\oost\mpl\aux_\template_arity_fwd.hpp"\
	".\oost\mpl\aux_\type_wrapper.hpp"\
	".\oost\mpl\aux_\value_wknd.hpp"\
	".\oost\mpl\aux_\yes_no.hpp"\
	".\oost\mpl\bool.hpp"\
	".\oost\mpl\bool_fwd.hpp"\
	".\oost\mpl\has_xxx.hpp"\
	".\oost\mpl\if.hpp"\
	".\oost\mpl\int.hpp"\
	".\oost\mpl\int_fwd.hpp"\
	".\oost\mpl\integral_c.hpp"\
	".\oost\mpl\integral_c_fwd.hpp"\
	".\oost\mpl\integral_c_tag.hpp"\
	".\oost\mpl\lambda_fwd.hpp"\
	".\oost\mpl\limits\arity.hpp"\
	".\oost\mpl\size_t.hpp"\
	".\oost\mpl\size_t_fwd.hpp"\
	".\oost\mpl\void_fwd.hpp"\
	".\oost\next_prior.hpp"\
	".\oost\non_type.hpp"\
	".\oost\noncopyable.hpp"\
	".\oost\preprocessor\arithmetic\add.hpp"\
	".\oost\preprocessor\arithmetic\inc.hpp"\
	".\oost\preprocessor\arithmetic\sub.hpp"\
	".\oost\preprocessor\cat.hpp"\
	".\oost\preprocessor\comma_if.hpp"\
	".\oost\preprocessor\config\config.hpp"\
	".\oost\preprocessor\control\detail\dmc\while.hpp"\
	".\oost\preprocessor\control\detail\edg\while.hpp"\
	".\oost\preprocessor\control\detail\msvc\while.hpp"\
	".\oost\preprocessor\control\detail\while.hpp"\
	".\oost\preprocessor\control\if.hpp"\
	".\oost\preprocessor\control\iif.hpp"\
	".\oost\preprocessor\control\while.hpp"\
	".\oost\preprocessor\debug\error.hpp"\
	".\oost\preprocessor\detail\auto_rec.hpp"\
	".\oost\preprocessor\detail\check.hpp"\
	".\oost\preprocessor\detail\dmc\auto_rec.hpp"\
	".\oost\preprocessor\detail\is_binary.hpp"\
	".\oost\preprocessor\empty.hpp"\
	".\oost\preprocessor\enum_params.hpp"\
	".\oost\preprocessor\facilities\empty.hpp"\
	".\oost\preprocessor\identity.hpp"\
	".\oost\preprocessor\inc.hpp"\
	".\oost\preprocessor\iterate.hpp"\
	".\oost\preprocessor\list\adt.hpp"\
	".\oost\preprocessor\list\append.hpp"\
	".\oost\preprocessor\list\detail\dmc\fold_left.hpp"\
	".\oost\preprocessor\list\detail\edg\fold_left.hpp"\
	".\oost\preprocessor\list\detail\edg\fold_right.hpp"\
	".\oost\preprocessor\list\detail\fold_left.hpp"\
	".\oost\preprocessor\list\detail\fold_right.hpp"\
	".\oost\preprocessor\list\fold_left.hpp"\
	".\oost\preprocessor\list\fold_right.hpp"\
	".\oost\preprocessor\list\for_each_i.hpp"\
	".\oost\preprocessor\list\reverse.hpp"\
	".\oost\preprocessor\list\transform.hpp"\
	".\oost\preprocessor\logical\and.hpp"\
	".\oost\preprocessor\logical\bitand.hpp"\
	".\oost\preprocessor\logical\bool.hpp"\
	".\oost\preprocessor\logical\compl.hpp"\
	".\oost\preprocessor\punctuation\comma.hpp"\
	".\oost\preprocessor\punctuation\comma_if.hpp"\
	".\oost\preprocessor\repeat.hpp"\
	".\oost\preprocessor\repetition\detail\dmc\for.hpp"\
	".\oost\preprocessor\repetition\detail\edg\for.hpp"\
	".\oost\preprocessor\repetition\detail\for.hpp"\
	".\oost\preprocessor\repetition\detail\msvc\for.hpp"\
	".\oost\preprocessor\repetition\enum_binary_params.hpp"\
	".\oost\preprocessor\repetition\enum_params.hpp"\
	".\oost\preprocessor\repetition\for.hpp"\
	".\oost\preprocessor\repetition\repeat.hpp"\
	".\oost\preprocessor\repetition\repeat_from_to.hpp"\
	".\oost\preprocessor\tuple\eat.hpp"\
	".\oost\preprocessor\tuple\elem.hpp"\
	".\oost\preprocessor\tuple\rem.hpp"\
	".\oost\preprocessor\tuple\to_list.hpp"\
	".\oost\regex.hpp"\
	".\oost\regex\config.hpp"\
	".\oost\regex\config\borland.hpp"\
	".\oost\regex\config\cwchar.hpp"\
	".\oost\regex\pattern_except.hpp"\
	".\oost\regex\pending\object_cache.hpp"\
	".\oost\regex\pending\static_mutex.hpp"\
	".\oost\regex\regex_traits.hpp"\
	".\oost\regex\v4\basic_regex.hpp"\
	".\oost\regex\v4\basic_regex_creator.hpp"\
	".\oost\regex\v4\basic_regex_parser.hpp"\
	".\oost\regex\v4\c_regex_traits.hpp"\
	".\oost\regex\v4\char_regex_traits.hpp"\
	".\oost\regex\v4\cpp_regex_traits.hpp"\
	".\oost\regex\v4\error_type.hpp"\
	".\oost\regex\v4\instances.hpp"\
	".\oost\regex\v4\iterator_category.hpp"\
	".\oost\regex\v4\iterator_traits.hpp"\
	".\oost\regex\v4\match_flags.hpp"\
	".\oost\regex\v4\match_results.hpp"\
	".\oost\regex\v4\perl_matcher.hpp"\
	".\oost\regex\v4\perl_matcher_common.hpp"\
	".\oost\regex\v4\perl_matcher_non_recursive.hpp"\
	".\oost\regex\v4\perl_matcher_recursive.hpp"\
	".\oost\regex\v4\primary_transform.hpp"\
	".\oost\regex\v4\protected_call.hpp"\
	".\oost\regex\v4\regbase.hpp"\
	".\oost\regex\v4\regex.hpp"\
	".\oost\regex\v4\regex_format.hpp"\
	".\oost\regex\v4\regex_fwd.hpp"\
	".\oost\regex\v4\regex_grep.hpp"\
	".\oost\regex\v4\regex_iterator.hpp"\
	".\oost\regex\v4\regex_match.hpp"\
	".\oost\regex\v4\regex_merge.hpp"\
	".\oost\regex\v4\regex_raw_buffer.hpp"\
	".\oost\regex\v4\regex_replace.hpp"\
	".\oost\regex\v4\regex_search.hpp"\
	".\oost\regex\v4\regex_split.hpp"\
	".\oost\regex\v4\regex_token_iterator.hpp"\
	".\oost\regex\v4\regex_traits.hpp"\
	".\oost\regex\v4\regex_traits_defaults.hpp"\
	".\oost\regex\v4\regex_workaround.hpp"\
	".\oost\regex\v4\states.hpp"\
	".\oost\regex\v4\sub_match.hpp"\
	".\oost\regex\v4\syntax_type.hpp"\
	".\oost\regex\v4\w32_regex_traits.hpp"\
	".\oost\regex_fwd.hpp"\
	".\oost\scoped_array.hpp"\
	".\oost\scoped_ptr.hpp"\
	".\oost\shared_ptr.hpp"\
	".\oost\static_assert.hpp"\
	".\oost\thread\detail\config.hpp"\
	".\oost\thread\detail\lock.hpp"\
	".\oost\thread\exceptions.hpp"\
	".\oost\thread\once.hpp"\
	".\oost\thread\recursive_mutex.hpp"\
	".\oost\throw_exception.hpp"\
	".\oost\type.hpp"\
	".\oost\type_traits\add_const.hpp"\
	".\oost\type_traits\add_reference.hpp"\
	".\oost\type_traits\alignment_of.hpp"\
	".\oost\type_traits\broken_compiler_spec.hpp"\
	".\oost\type_traits\config.hpp"\
	".\oost\type_traits\detail\bool_trait_def.hpp"\
	".\oost\type_traits\detail\bool_trait_undef.hpp"\
	".\oost\type_traits\detail\cv_traits_impl.hpp"\
	".\oost\type_traits\detail\false_result.hpp"\
	".\oost\type_traits\detail\ice_and.hpp"\
	".\oost\type_traits\detail\ice_eq.hpp"\
	".\oost\type_traits\detail\ice_not.hpp"\
	".\oost\type_traits\detail\ice_or.hpp"\
	".\oost\type_traits\detail\is_function_ptr_helper.hpp"\
	".\oost\type_traits\detail\is_function_ptr_tester.hpp"\
	".\oost\type_traits\detail\is_mem_fun_pointer_impl.hpp"\
	".\oost\type_traits\detail\is_mem_fun_pointer_tester.hpp"\
	".\oost\type_traits\detail\size_t_trait_def.hpp"\
	".\oost\type_traits\detail\size_t_trait_undef.hpp"\
	".\oost\type_traits\detail\template_arity_spec.hpp"\
	".\oost\type_traits\detail\type_trait_def.hpp"\
	".\oost\type_traits\detail\type_trait_undef.hpp"\
	".\oost\type_traits\detail\wrap.hpp"\
	".\oost\type_traits\detail\yes_no_type.hpp"\
	".\oost\type_traits\has_trivial_assign.hpp"\
	".\oost\type_traits\has_trivial_constructor.hpp"\
	".\oost\type_traits\has_trivial_copy.hpp"\
	".\oost\type_traits\has_trivial_destructor.hpp"\
	".\oost\type_traits\ice.hpp"\
	".\oost\type_traits\integral_constant.hpp"\
	".\oost\type_traits\intrinsics.hpp"\
	".\oost\type_traits\is_abstract.hpp"\
	".\oost\type_traits\is_arithmetic.hpp"\
	".\oost\type_traits\is_array.hpp"\
	".\oost\type_traits\is_class.hpp"\
	".\oost\type_traits\is_const.hpp"\
	".\oost\type_traits\is_convertible.hpp"\
	".\oost\type_traits\is_enum.hpp"\
	".\oost\type_traits\is_float.hpp"\
	".\oost\type_traits\is_function.hpp"\
	".\oost\type_traits\is_integral.hpp"\
	".\oost\type_traits\is_member_function_pointer.hpp"\
	".\oost\type_traits\is_member_pointer.hpp"\
	".\oost\type_traits\is_pod.hpp"\
	".\oost\type_traits\is_pointer.hpp"\
	".\oost\type_traits\is_polymorphic.hpp"\
	".\oost\type_traits\is_reference.hpp"\
	".\oost\type_traits\is_same.hpp"\
	".\oost\type_traits\is_scalar.hpp"\
	".\oost\type_traits\is_union.hpp"\
	".\oost\type_traits\is_void.hpp"\
	".\oost\type_traits\is_volatile.hpp"\
	".\oost\type_traits\msvc\remove_bounds.hpp"\
	".\oost\type_traits\msvc\remove_cv.hpp"\
	".\oost\type_traits\msvc\remove_reference.hpp"\
	".\oost\type_traits\msvc\typeof.hpp"\
	".\oost\type_traits\remove_bounds.hpp"\
	".\oost\type_traits\remove_cv.hpp"\
	".\oost\type_traits\remove_reference.hpp"\
	".\oost\type_traits\type_with_alignment.hpp"\
	".\oost\utility.hpp"\
	".\oost\utility\addressof.hpp"\
	".\oost\utility\base_from_member.hpp"\
	".\oost\utility\enable_if.hpp"\
	".\oost\version.hpp"\
	".\sing\cstring"\
	".\thread.h"\
	".\tl\_abbrevs.h"\
	".\tl\_algobase.c"\
	".\tl\_algobase.h"\
	".\tl\_alloc.c"\
	".\tl\_alloc.h"\
	".\tl\_cmath.h"\
	".\tl\_config_compat_post.h"\
	".\tl\_construct.h"\
	".\tl\_cprolog.h"\
	".\tl\_cstdarg.h"\
	".\tl\_cstddef.h"\
	".\tl\_cstdio.h"\
	".\tl\_cstdlib.h"\
	".\tl\_cstring.h"\
	".\tl\_ctime.h"\
	".\tl\_ctraits_fns.h"\
	".\tl\_cwchar.h"\
	".\tl\_epilog.h"\
	".\tl\_exception.h"\
	".\tl\_function.h"\
	".\tl\_function_adaptors.h"\
	".\tl\_function_base.h"\
	".\tl\_iosfwd.h"\
	".\tl\_iterator.h"\
	".\tl\_iterator_base.h"\
	".\tl\_iterator_old.h"\
	".\tl\_mbstate_t.h"\
	".\tl\_move_construct_fwk.h"\
	".\tl\_new.h"\
	".\tl\_pair.h"\
	".\tl\_prolog.h"\
	".\tl\_pthread_alloc.h"\
	".\tl\_ptrs_specialize.h"\
	".\tl\_range_errors.c"\
	".\tl\_range_errors.h"\
	".\tl\_sparc_atomic.h"\
	".\tl\_stdexcept.h"\
	".\tl\_stdexcept_base.c"\
	".\tl\_stdexcept_base.h"\
	".\tl\_stlport_version.h"\
	".\tl\_string.c"\
	".\tl\_string.h"\
	".\tl\_string_base.h"\
	".\tl\_string_fwd.h"\
	".\tl\_string_npos.h"\
	".\tl\_string_operators.h"\
	".\tl\_string_sum.h"\
	".\tl\_string_sum_methods.h"\
	".\tl\_string_workaround.h"\
	".\tl\_threads.c"\
	".\tl\_threads.h"\
	".\tl\_uninitialized.h"\
	".\tl\boost_type_traits.h"\
	".\tl\char_traits.h"\
	".\tl\config\_aix.h"\
	".\tl\config\_apcc.h"\
	".\tl\config\_as400.h"\
	".\tl\config\_auto_link.h"\
	".\tl\config\_bc.h"\
	".\tl\config\_como.h"\
	".\tl\config\_cygwin.h"\
	".\tl\config\_dec.h"\
	".\tl\config\_dec_vms.h"\
	".\tl\config\_detect_dll_or_lib.h"\
	".\tl\config\_dm.h"\
	".\tl\config\_epilog.h"\
	".\tl\config\_evc.h"\
	".\tl\config\_feedback.h"\
	".\tl\config\_freebsd.h"\
	".\tl\config\_fujitsu.h"\
	".\tl\config\_gcc.h"\
	".\tl\config\_hpacc.h"\
	".\tl\config\_hpux.h"\
	".\tl\config\_ibm.h"\
	".\tl\config\_icc.h"\
	".\tl\config\_intel.h"\
	".\tl\config\_kai.h"\
	".\tl\config\_linux.h"\
	".\tl\config\_mac.h"\
	".\tl\config\_macosx.h"\
	".\tl\config\_msvc.h"\
	".\tl\config\_mwerks.h"\
	".\tl\config\_native_headers.h"\
	".\tl\config\_openbsd.h"\
	".\tl\config\_prolog.h"\
	".\tl\config\_sgi.h"\
	".\tl\config\_solaris.h"\
	".\tl\config\_sunprocc.h"\
	".\tl\config\_system.h"\
	".\tl\config\_warnings_off.h"\
	".\tl\config\_watcom.h"\
	".\tl\config\_windows.h"\
	".\tl\config\compat.h"\
	".\tl\config\features.h"\
	".\tl\config\host.h"\
	".\tl\config\stl_confix.h"\
	".\tl\config\stl_mycomp.h"\
	".\tl\config\user_config.h"\
	".\tl\debug\_debug.c"\
	".\tl\debug\_debug.h"\
	".\tl\debug\_iterator.h"\
	".\tl\debug\_string.h"\
	".\tl\debug\_string_sum_methods.h"\
	".\tl\msl_string.h"\
	".\tl\type_manips.h"\
	".\tl\type_traits.h"\
	"D:\bhj\boost_1_34_1\boost\thread\scoped_critical_region.hpp"\
	
# End Source File
# Begin Source File

SOURCE=.\HListBox.cpp
# End Source File
# Begin Source File

SOURCE=.\RunBhjRun.cpp
DEP_CPP_RUNBH=\
	".\bhjlib.h"\
	".\EkbEdit.h"\
	".\RunBhjRun.h"\
	".\RunBhjRunDlg.h"\
	".\simplewnd.h"\
	".\StdAfx.h"\
	
# End Source File
# Begin Source File

SOURCE=.\RunBhjRun.rc
# End Source File
# Begin Source File

SOURCE=.\RunBhjRunDlg.cpp
DEP_CPP_RUNBHJ=\
	".\bhjlib.h"\
	".\EkbEdit.h"\
	".\RunBhjRun.h"\
	".\RunBhjRunDlg.h"\
	".\simplewnd.h"\
	".\StdAfx.h"\
	
# End Source File
# Begin Source File

SOURCE=.\StdAfx.cpp
DEP_CPP_STDAF=\
	".\bhjlib.h"\
	".\StdAfx.h"\
	
# ADD CPP /Yc"stdafx.h"
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\bhjlib.h
# End Source File
# Begin Source File

SOURCE=.\EkbEdit.h
# End Source File
# Begin Source File

SOURCE=.\HListBox.h
# End Source File
# Begin Source File

SOURCE=.\Resource.h
# End Source File
# Begin Source File

SOURCE=.\RunBhjRun.h
# End Source File
# Begin Source File

SOURCE=.\RunBhjRunDlg.h
# End Source File
# Begin Source File

SOURCE=.\StdAfx.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\res\RunBhjRun.ico
# End Source File
# Begin Source File

SOURCE=.\res\RunBhjRun.rc2
# End Source File
# End Group
# Begin Source File

SOURCE=.\ReadMe.txt
# End Source File
# End Target
# End Project
