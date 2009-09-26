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
	{$(INCLUDE)}"boost\assert.hpp"\
	{$(INCLUDE)}"boost\checked_delete.hpp"\
	{$(INCLUDE)}"boost\config.hpp"\
	{$(INCLUDE)}"boost\config\auto_link.hpp"\
	{$(INCLUDE)}"boost\config\no_tr1\utility.hpp"\
	{$(INCLUDE)}"boost\config\posix_features.hpp"\
	{$(INCLUDE)}"boost\config\requires_threads.hpp"\
	{$(INCLUDE)}"boost\config\select_compiler_config.hpp"\
	{$(INCLUDE)}"boost\config\select_platform_config.hpp"\
	{$(INCLUDE)}"boost\config\select_stdlib_config.hpp"\
	{$(INCLUDE)}"boost\config\suffix.hpp"\
	{$(INCLUDE)}"boost\cstdint.hpp"\
	{$(INCLUDE)}"boost\current_function.hpp"\
	{$(INCLUDE)}"boost\detail\atomic_count.hpp"\
	{$(INCLUDE)}"boost\detail\atomic_count_gcc.hpp"\
	{$(INCLUDE)}"boost\detail\atomic_count_pthreads.hpp"\
	{$(INCLUDE)}"boost\detail\atomic_count_win32.hpp"\
	{$(INCLUDE)}"boost\detail\bad_weak_ptr.hpp"\
	{$(INCLUDE)}"boost\detail\endian.hpp"\
	{$(INCLUDE)}"boost\detail\interlocked.hpp"\
	{$(INCLUDE)}"boost\detail\lightweight_mutex.hpp"\
	{$(INCLUDE)}"boost\detail\limits.hpp"\
	{$(INCLUDE)}"boost\detail\lwm_nop.hpp"\
	{$(INCLUDE)}"boost\detail\lwm_pthreads.hpp"\
	{$(INCLUDE)}"boost\detail\lwm_win32_cs.hpp"\
	{$(INCLUDE)}"boost\detail\quick_allocator.hpp"\
	{$(INCLUDE)}"boost\detail\shared_count.hpp"\
	{$(INCLUDE)}"boost\detail\shared_ptr_nmt.hpp"\
	{$(INCLUDE)}"boost\detail\sp_counted_base.hpp"\
	{$(INCLUDE)}"boost\detail\sp_counted_base_cw_ppc.hpp"\
	{$(INCLUDE)}"boost\detail\sp_counted_base_gcc_ia64.hpp"\
	{$(INCLUDE)}"boost\detail\sp_counted_base_gcc_ppc.hpp"\
	{$(INCLUDE)}"boost\detail\sp_counted_base_gcc_x86.hpp"\
	{$(INCLUDE)}"boost\detail\sp_counted_base_nt.hpp"\
	{$(INCLUDE)}"boost\detail\sp_counted_base_pt.hpp"\
	{$(INCLUDE)}"boost\detail\sp_counted_base_w32.hpp"\
	{$(INCLUDE)}"boost\detail\sp_counted_impl.hpp"\
	{$(INCLUDE)}"boost\detail\workaround.hpp"\
	{$(INCLUDE)}"boost\limits.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\adl_barrier.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\arity.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\adl.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\arrays.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\ctps.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\dtp.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\eti.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\gcc.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\has_xxx.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\integral.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\intel.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\lambda.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\msvc.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\msvc_typename.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\nttp.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\overload_resolution.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\preprocessor.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\static_constant.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\ttp.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\workaround.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\integral_wrapper.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\lambda_arity_param.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\lambda_support.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\na.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\na_fwd.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\na_spec.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\nttp_decl.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\preprocessor\def_params_tail.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\preprocessor\enum.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\preprocessor\filter_params.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\preprocessor\params.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\preprocessor\sub.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\preprocessor\tuple.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\static_cast.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\template_arity_fwd.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\type_wrapper.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\value_wknd.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\yes_no.hpp"\
	{$(INCLUDE)}"boost\mpl\bool.hpp"\
	{$(INCLUDE)}"boost\mpl\bool_fwd.hpp"\
	{$(INCLUDE)}"boost\mpl\has_xxx.hpp"\
	{$(INCLUDE)}"boost\mpl\if.hpp"\
	{$(INCLUDE)}"boost\mpl\int.hpp"\
	{$(INCLUDE)}"boost\mpl\int_fwd.hpp"\
	{$(INCLUDE)}"boost\mpl\integral_c.hpp"\
	{$(INCLUDE)}"boost\mpl\integral_c_fwd.hpp"\
	{$(INCLUDE)}"boost\mpl\integral_c_tag.hpp"\
	{$(INCLUDE)}"boost\mpl\lambda_fwd.hpp"\
	{$(INCLUDE)}"boost\mpl\limits\arity.hpp"\
	{$(INCLUDE)}"boost\mpl\size_t.hpp"\
	{$(INCLUDE)}"boost\mpl\size_t_fwd.hpp"\
	{$(INCLUDE)}"boost\mpl\void_fwd.hpp"\
	{$(INCLUDE)}"boost\next_prior.hpp"\
	{$(INCLUDE)}"boost\non_type.hpp"\
	{$(INCLUDE)}"boost\noncopyable.hpp"\
	{$(INCLUDE)}"boost\preprocessor\arithmetic\add.hpp"\
	{$(INCLUDE)}"boost\preprocessor\arithmetic\inc.hpp"\
	{$(INCLUDE)}"boost\preprocessor\arithmetic\sub.hpp"\
	{$(INCLUDE)}"boost\preprocessor\cat.hpp"\
	{$(INCLUDE)}"boost\preprocessor\comma_if.hpp"\
	{$(INCLUDE)}"boost\preprocessor\config\config.hpp"\
	{$(INCLUDE)}"boost\preprocessor\control\detail\dmc\while.hpp"\
	{$(INCLUDE)}"boost\preprocessor\control\detail\edg\while.hpp"\
	{$(INCLUDE)}"boost\preprocessor\control\detail\msvc\while.hpp"\
	{$(INCLUDE)}"boost\preprocessor\control\detail\while.hpp"\
	{$(INCLUDE)}"boost\preprocessor\control\if.hpp"\
	{$(INCLUDE)}"boost\preprocessor\control\iif.hpp"\
	{$(INCLUDE)}"boost\preprocessor\control\while.hpp"\
	{$(INCLUDE)}"boost\preprocessor\debug\error.hpp"\
	{$(INCLUDE)}"boost\preprocessor\detail\auto_rec.hpp"\
	{$(INCLUDE)}"boost\preprocessor\detail\check.hpp"\
	{$(INCLUDE)}"boost\preprocessor\detail\dmc\auto_rec.hpp"\
	{$(INCLUDE)}"boost\preprocessor\detail\is_binary.hpp"\
	{$(INCLUDE)}"boost\preprocessor\empty.hpp"\
	{$(INCLUDE)}"boost\preprocessor\enum_params.hpp"\
	{$(INCLUDE)}"boost\preprocessor\facilities\empty.hpp"\
	{$(INCLUDE)}"boost\preprocessor\identity.hpp"\
	{$(INCLUDE)}"boost\preprocessor\inc.hpp"\
	{$(INCLUDE)}"boost\preprocessor\iterate.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\adt.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\append.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\detail\dmc\fold_left.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\detail\edg\fold_left.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\detail\edg\fold_right.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\detail\fold_left.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\detail\fold_right.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\fold_left.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\fold_right.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\for_each_i.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\reverse.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\transform.hpp"\
	{$(INCLUDE)}"boost\preprocessor\logical\and.hpp"\
	{$(INCLUDE)}"boost\preprocessor\logical\bitand.hpp"\
	{$(INCLUDE)}"boost\preprocessor\logical\bool.hpp"\
	{$(INCLUDE)}"boost\preprocessor\logical\compl.hpp"\
	{$(INCLUDE)}"boost\preprocessor\punctuation\comma.hpp"\
	{$(INCLUDE)}"boost\preprocessor\punctuation\comma_if.hpp"\
	{$(INCLUDE)}"boost\preprocessor\repeat.hpp"\
	{$(INCLUDE)}"boost\preprocessor\repetition\detail\dmc\for.hpp"\
	{$(INCLUDE)}"boost\preprocessor\repetition\detail\edg\for.hpp"\
	{$(INCLUDE)}"boost\preprocessor\repetition\detail\for.hpp"\
	{$(INCLUDE)}"boost\preprocessor\repetition\detail\msvc\for.hpp"\
	{$(INCLUDE)}"boost\preprocessor\repetition\enum_binary_params.hpp"\
	{$(INCLUDE)}"boost\preprocessor\repetition\enum_params.hpp"\
	{$(INCLUDE)}"boost\preprocessor\repetition\for.hpp"\
	{$(INCLUDE)}"boost\preprocessor\repetition\repeat.hpp"\
	{$(INCLUDE)}"boost\preprocessor\repetition\repeat_from_to.hpp"\
	{$(INCLUDE)}"boost\preprocessor\tuple\eat.hpp"\
	{$(INCLUDE)}"boost\preprocessor\tuple\elem.hpp"\
	{$(INCLUDE)}"boost\preprocessor\tuple\rem.hpp"\
	{$(INCLUDE)}"boost\preprocessor\tuple\to_list.hpp"\
	{$(INCLUDE)}"boost\regex.hpp"\
	{$(INCLUDE)}"boost\regex\config.hpp"\
	{$(INCLUDE)}"boost\regex\config\borland.hpp"\
	{$(INCLUDE)}"boost\regex\config\cwchar.hpp"\
	{$(INCLUDE)}"boost\regex\pattern_except.hpp"\
	{$(INCLUDE)}"boost\regex\pending\object_cache.hpp"\
	{$(INCLUDE)}"boost\regex\pending\static_mutex.hpp"\
	{$(INCLUDE)}"boost\regex\regex_traits.hpp"\
	{$(INCLUDE)}"boost\regex\v4\basic_regex.hpp"\
	{$(INCLUDE)}"boost\regex\v4\basic_regex_creator.hpp"\
	{$(INCLUDE)}"boost\regex\v4\basic_regex_parser.hpp"\
	{$(INCLUDE)}"boost\regex\v4\c_regex_traits.hpp"\
	{$(INCLUDE)}"boost\regex\v4\char_regex_traits.hpp"\
	{$(INCLUDE)}"boost\regex\v4\cpp_regex_traits.hpp"\
	{$(INCLUDE)}"boost\regex\v4\error_type.hpp"\
	{$(INCLUDE)}"boost\regex\v4\instances.hpp"\
	{$(INCLUDE)}"boost\regex\v4\iterator_category.hpp"\
	{$(INCLUDE)}"boost\regex\v4\iterator_traits.hpp"\
	{$(INCLUDE)}"boost\regex\v4\match_flags.hpp"\
	{$(INCLUDE)}"boost\regex\v4\match_results.hpp"\
	{$(INCLUDE)}"boost\regex\v4\perl_matcher.hpp"\
	{$(INCLUDE)}"boost\regex\v4\perl_matcher_common.hpp"\
	{$(INCLUDE)}"boost\regex\v4\perl_matcher_non_recursive.hpp"\
	{$(INCLUDE)}"boost\regex\v4\perl_matcher_recursive.hpp"\
	{$(INCLUDE)}"boost\regex\v4\primary_transform.hpp"\
	{$(INCLUDE)}"boost\regex\v4\protected_call.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regbase.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_format.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_fwd.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_grep.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_iterator.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_match.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_merge.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_raw_buffer.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_replace.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_search.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_split.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_token_iterator.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_traits.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_traits_defaults.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_workaround.hpp"\
	{$(INCLUDE)}"boost\regex\v4\states.hpp"\
	{$(INCLUDE)}"boost\regex\v4\sub_match.hpp"\
	{$(INCLUDE)}"boost\regex\v4\syntax_type.hpp"\
	{$(INCLUDE)}"boost\regex\v4\w32_regex_traits.hpp"\
	{$(INCLUDE)}"boost\regex_fwd.hpp"\
	{$(INCLUDE)}"boost\scoped_array.hpp"\
	{$(INCLUDE)}"boost\scoped_ptr.hpp"\
	{$(INCLUDE)}"boost\shared_ptr.hpp"\
	{$(INCLUDE)}"boost\static_assert.hpp"\
	{$(INCLUDE)}"boost\thread\detail\config.hpp"\
	{$(INCLUDE)}"boost\thread\detail\lock.hpp"\
	{$(INCLUDE)}"boost\thread\exceptions.hpp"\
	{$(INCLUDE)}"boost\thread\once.hpp"\
	{$(INCLUDE)}"boost\thread\recursive_mutex.hpp"\
	{$(INCLUDE)}"boost\throw_exception.hpp"\
	{$(INCLUDE)}"boost\type.hpp"\
	{$(INCLUDE)}"boost\type_traits\add_const.hpp"\
	{$(INCLUDE)}"boost\type_traits\add_reference.hpp"\
	{$(INCLUDE)}"boost\type_traits\alignment_of.hpp"\
	{$(INCLUDE)}"boost\type_traits\broken_compiler_spec.hpp"\
	{$(INCLUDE)}"boost\type_traits\config.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\bool_trait_def.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\bool_trait_undef.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\cv_traits_impl.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\false_result.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\ice_and.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\ice_eq.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\ice_not.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\ice_or.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\is_function_ptr_helper.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\is_function_ptr_tester.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\is_mem_fun_pointer_impl.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\is_mem_fun_pointer_tester.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\size_t_trait_def.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\size_t_trait_undef.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\template_arity_spec.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\type_trait_def.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\type_trait_undef.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\wrap.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\yes_no_type.hpp"\
	{$(INCLUDE)}"boost\type_traits\has_trivial_assign.hpp"\
	{$(INCLUDE)}"boost\type_traits\has_trivial_constructor.hpp"\
	{$(INCLUDE)}"boost\type_traits\has_trivial_copy.hpp"\
	{$(INCLUDE)}"boost\type_traits\has_trivial_destructor.hpp"\
	{$(INCLUDE)}"boost\type_traits\ice.hpp"\
	{$(INCLUDE)}"boost\type_traits\integral_constant.hpp"\
	{$(INCLUDE)}"boost\type_traits\intrinsics.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_abstract.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_arithmetic.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_array.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_class.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_const.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_convertible.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_enum.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_float.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_function.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_integral.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_member_function_pointer.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_member_pointer.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_pod.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_pointer.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_polymorphic.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_reference.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_same.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_scalar.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_union.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_void.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_volatile.hpp"\
	{$(INCLUDE)}"boost\type_traits\msvc\remove_bounds.hpp"\
	{$(INCLUDE)}"boost\type_traits\msvc\remove_cv.hpp"\
	{$(INCLUDE)}"boost\type_traits\msvc\remove_reference.hpp"\
	{$(INCLUDE)}"boost\type_traits\msvc\typeof.hpp"\
	{$(INCLUDE)}"boost\type_traits\remove_bounds.hpp"\
	{$(INCLUDE)}"boost\type_traits\remove_cv.hpp"\
	{$(INCLUDE)}"boost\type_traits\remove_reference.hpp"\
	{$(INCLUDE)}"boost\type_traits\type_with_alignment.hpp"\
	{$(INCLUDE)}"boost\utility.hpp"\
	{$(INCLUDE)}"boost\utility\addressof.hpp"\
	{$(INCLUDE)}"boost\utility\base_from_member.hpp"\
	{$(INCLUDE)}"boost\utility\enable_if.hpp"\
	{$(INCLUDE)}"boost\version.hpp"\
	{$(INCLUDE)}"mem.h"\
	{$(INCLUDE)}"pthread.h"\
	{$(INCLUDE)}"stl\_abbrevs.h"\
	{$(INCLUDE)}"stl\_algobase.c"\
	{$(INCLUDE)}"stl\_algobase.h"\
	{$(INCLUDE)}"stl\_alloc.c"\
	{$(INCLUDE)}"stl\_alloc.h"\
	{$(INCLUDE)}"stl\_cmath.h"\
	{$(INCLUDE)}"stl\_config_compat_post.h"\
	{$(INCLUDE)}"stl\_construct.h"\
	{$(INCLUDE)}"stl\_cprolog.h"\
	{$(INCLUDE)}"stl\_cstdarg.h"\
	{$(INCLUDE)}"stl\_cstddef.h"\
	{$(INCLUDE)}"stl\_cstdio.h"\
	{$(INCLUDE)}"stl\_cstdlib.h"\
	{$(INCLUDE)}"stl\_cstring.h"\
	{$(INCLUDE)}"stl\_ctime.h"\
	{$(INCLUDE)}"stl\_ctraits_fns.h"\
	{$(INCLUDE)}"stl\_cwchar.h"\
	{$(INCLUDE)}"stl\_epilog.h"\
	{$(INCLUDE)}"stl\_exception.h"\
	{$(INCLUDE)}"stl\_function.h"\
	{$(INCLUDE)}"stl\_function_adaptors.h"\
	{$(INCLUDE)}"stl\_function_base.h"\
	{$(INCLUDE)}"stl\_iosfwd.h"\
	{$(INCLUDE)}"stl\_iterator.h"\
	{$(INCLUDE)}"stl\_iterator_base.h"\
	{$(INCLUDE)}"stl\_iterator_old.h"\
	{$(INCLUDE)}"stl\_mbstate_t.h"\
	{$(INCLUDE)}"stl\_move_construct_fwk.h"\
	{$(INCLUDE)}"stl\_new.h"\
	{$(INCLUDE)}"stl\_pair.h"\
	{$(INCLUDE)}"stl\_prolog.h"\
	{$(INCLUDE)}"stl\_pthread_alloc.h"\
	{$(INCLUDE)}"stl\_ptrs_specialize.h"\
	{$(INCLUDE)}"stl\_range_errors.c"\
	{$(INCLUDE)}"stl\_range_errors.h"\
	{$(INCLUDE)}"stl\_sparc_atomic.h"\
	{$(INCLUDE)}"stl\_stdexcept.h"\
	{$(INCLUDE)}"stl\_stdexcept_base.c"\
	{$(INCLUDE)}"stl\_stdexcept_base.h"\
	{$(INCLUDE)}"stl\_stlport_version.h"\
	{$(INCLUDE)}"stl\_string.c"\
	{$(INCLUDE)}"stl\_string.h"\
	{$(INCLUDE)}"stl\_string_base.h"\
	{$(INCLUDE)}"stl\_string_fwd.h"\
	{$(INCLUDE)}"stl\_string_npos.h"\
	{$(INCLUDE)}"stl\_string_operators.h"\
	{$(INCLUDE)}"stl\_string_sum.h"\
	{$(INCLUDE)}"stl\_string_sum_methods.h"\
	{$(INCLUDE)}"stl\_string_workaround.h"\
	{$(INCLUDE)}"stl\_threads.c"\
	{$(INCLUDE)}"stl\_threads.h"\
	{$(INCLUDE)}"stl\_uninitialized.h"\
	{$(INCLUDE)}"stl\boost_type_traits.h"\
	{$(INCLUDE)}"stl\char_traits.h"\
	{$(INCLUDE)}"stl\config\_aix.h"\
	{$(INCLUDE)}"stl\config\_apcc.h"\
	{$(INCLUDE)}"stl\config\_as400.h"\
	{$(INCLUDE)}"stl\config\_auto_link.h"\
	{$(INCLUDE)}"stl\config\_bc.h"\
	{$(INCLUDE)}"stl\config\_como.h"\
	{$(INCLUDE)}"stl\config\_cygwin.h"\
	{$(INCLUDE)}"stl\config\_dec.h"\
	{$(INCLUDE)}"stl\config\_dec_vms.h"\
	{$(INCLUDE)}"stl\config\_detect_dll_or_lib.h"\
	{$(INCLUDE)}"stl\config\_dm.h"\
	{$(INCLUDE)}"stl\config\_epilog.h"\
	{$(INCLUDE)}"stl\config\_evc.h"\
	{$(INCLUDE)}"stl\config\_feedback.h"\
	{$(INCLUDE)}"stl\config\_freebsd.h"\
	{$(INCLUDE)}"stl\config\_fujitsu.h"\
	{$(INCLUDE)}"stl\config\_gcc.h"\
	{$(INCLUDE)}"stl\config\_hpacc.h"\
	{$(INCLUDE)}"stl\config\_hpux.h"\
	{$(INCLUDE)}"stl\config\_ibm.h"\
	{$(INCLUDE)}"stl\config\_icc.h"\
	{$(INCLUDE)}"stl\config\_intel.h"\
	{$(INCLUDE)}"stl\config\_kai.h"\
	{$(INCLUDE)}"stl\config\_linux.h"\
	{$(INCLUDE)}"stl\config\_mac.h"\
	{$(INCLUDE)}"stl\config\_macosx.h"\
	{$(INCLUDE)}"stl\config\_msvc.h"\
	{$(INCLUDE)}"stl\config\_mwerks.h"\
	{$(INCLUDE)}"stl\config\_native_headers.h"\
	{$(INCLUDE)}"stl\config\_openbsd.h"\
	{$(INCLUDE)}"stl\config\_prolog.h"\
	{$(INCLUDE)}"stl\config\_sgi.h"\
	{$(INCLUDE)}"stl\config\_solaris.h"\
	{$(INCLUDE)}"stl\config\_sunprocc.h"\
	{$(INCLUDE)}"stl\config\_system.h"\
	{$(INCLUDE)}"stl\config\_warnings_off.h"\
	{$(INCLUDE)}"stl\config\_watcom.h"\
	{$(INCLUDE)}"stl\config\_windows.h"\
	{$(INCLUDE)}"stl\config\compat.h"\
	{$(INCLUDE)}"stl\config\features.h"\
	{$(INCLUDE)}"stl\config\host.h"\
	{$(INCLUDE)}"stl\config\stl_confix.h"\
	{$(INCLUDE)}"stl\config\stl_mycomp.h"\
	{$(INCLUDE)}"stl\config\user_config.h"\
	{$(INCLUDE)}"stl\debug\_debug.c"\
	{$(INCLUDE)}"stl\debug\_debug.h"\
	{$(INCLUDE)}"stl\debug\_iterator.h"\
	{$(INCLUDE)}"stl\debug\_string.h"\
	{$(INCLUDE)}"stl\debug\_string_sum_methods.h"\
	{$(INCLUDE)}"stl\msl_string.h"\
	{$(INCLUDE)}"stl\type_manips.h"\
	{$(INCLUDE)}"stl\type_traits.h"\
	{$(INCLUDE)}"using\cstring"\
	
NODEP_CPP_BHJLI=\
	"..\..\usr\include\pthread.h"\
	"..\..\usr\include\sys\cdefs.h"\
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
	{$(INCLUDE)}"bhjdebug.h"\
	{$(INCLUDE)}"boost\assert.hpp"\
	{$(INCLUDE)}"boost\checked_delete.hpp"\
	{$(INCLUDE)}"boost\config.hpp"\
	{$(INCLUDE)}"boost\config\auto_link.hpp"\
	{$(INCLUDE)}"boost\config\no_tr1\utility.hpp"\
	{$(INCLUDE)}"boost\config\posix_features.hpp"\
	{$(INCLUDE)}"boost\config\requires_threads.hpp"\
	{$(INCLUDE)}"boost\config\select_compiler_config.hpp"\
	{$(INCLUDE)}"boost\config\select_platform_config.hpp"\
	{$(INCLUDE)}"boost\config\select_stdlib_config.hpp"\
	{$(INCLUDE)}"boost\config\suffix.hpp"\
	{$(INCLUDE)}"boost\cstdint.hpp"\
	{$(INCLUDE)}"boost\current_function.hpp"\
	{$(INCLUDE)}"boost\detail\atomic_count.hpp"\
	{$(INCLUDE)}"boost\detail\atomic_count_gcc.hpp"\
	{$(INCLUDE)}"boost\detail\atomic_count_pthreads.hpp"\
	{$(INCLUDE)}"boost\detail\atomic_count_win32.hpp"\
	{$(INCLUDE)}"boost\detail\bad_weak_ptr.hpp"\
	{$(INCLUDE)}"boost\detail\endian.hpp"\
	{$(INCLUDE)}"boost\detail\interlocked.hpp"\
	{$(INCLUDE)}"boost\detail\lightweight_mutex.hpp"\
	{$(INCLUDE)}"boost\detail\limits.hpp"\
	{$(INCLUDE)}"boost\detail\lwm_nop.hpp"\
	{$(INCLUDE)}"boost\detail\lwm_pthreads.hpp"\
	{$(INCLUDE)}"boost\detail\lwm_win32_cs.hpp"\
	{$(INCLUDE)}"boost\detail\quick_allocator.hpp"\
	{$(INCLUDE)}"boost\detail\shared_count.hpp"\
	{$(INCLUDE)}"boost\detail\shared_ptr_nmt.hpp"\
	{$(INCLUDE)}"boost\detail\sp_counted_base.hpp"\
	{$(INCLUDE)}"boost\detail\sp_counted_base_cw_ppc.hpp"\
	{$(INCLUDE)}"boost\detail\sp_counted_base_gcc_ia64.hpp"\
	{$(INCLUDE)}"boost\detail\sp_counted_base_gcc_ppc.hpp"\
	{$(INCLUDE)}"boost\detail\sp_counted_base_gcc_x86.hpp"\
	{$(INCLUDE)}"boost\detail\sp_counted_base_nt.hpp"\
	{$(INCLUDE)}"boost\detail\sp_counted_base_pt.hpp"\
	{$(INCLUDE)}"boost\detail\sp_counted_base_w32.hpp"\
	{$(INCLUDE)}"boost\detail\sp_counted_impl.hpp"\
	{$(INCLUDE)}"boost\detail\workaround.hpp"\
	{$(INCLUDE)}"boost\limits.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\adl_barrier.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\arity.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\adl.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\arrays.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\ctps.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\dtp.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\eti.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\gcc.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\has_xxx.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\integral.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\intel.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\lambda.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\msvc.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\msvc_typename.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\nttp.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\overload_resolution.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\preprocessor.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\static_constant.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\ttp.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\config\workaround.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\integral_wrapper.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\lambda_arity_param.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\lambda_support.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\na.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\na_fwd.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\na_spec.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\nttp_decl.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\preprocessor\def_params_tail.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\preprocessor\enum.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\preprocessor\filter_params.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\preprocessor\params.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\preprocessor\sub.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\preprocessor\tuple.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\static_cast.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\template_arity_fwd.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\type_wrapper.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\value_wknd.hpp"\
	{$(INCLUDE)}"boost\mpl\aux_\yes_no.hpp"\
	{$(INCLUDE)}"boost\mpl\bool.hpp"\
	{$(INCLUDE)}"boost\mpl\bool_fwd.hpp"\
	{$(INCLUDE)}"boost\mpl\has_xxx.hpp"\
	{$(INCLUDE)}"boost\mpl\if.hpp"\
	{$(INCLUDE)}"boost\mpl\int.hpp"\
	{$(INCLUDE)}"boost\mpl\int_fwd.hpp"\
	{$(INCLUDE)}"boost\mpl\integral_c.hpp"\
	{$(INCLUDE)}"boost\mpl\integral_c_fwd.hpp"\
	{$(INCLUDE)}"boost\mpl\integral_c_tag.hpp"\
	{$(INCLUDE)}"boost\mpl\lambda_fwd.hpp"\
	{$(INCLUDE)}"boost\mpl\limits\arity.hpp"\
	{$(INCLUDE)}"boost\mpl\size_t.hpp"\
	{$(INCLUDE)}"boost\mpl\size_t_fwd.hpp"\
	{$(INCLUDE)}"boost\mpl\void_fwd.hpp"\
	{$(INCLUDE)}"boost\next_prior.hpp"\
	{$(INCLUDE)}"boost\non_type.hpp"\
	{$(INCLUDE)}"boost\noncopyable.hpp"\
	{$(INCLUDE)}"boost\preprocessor\arithmetic\add.hpp"\
	{$(INCLUDE)}"boost\preprocessor\arithmetic\inc.hpp"\
	{$(INCLUDE)}"boost\preprocessor\arithmetic\sub.hpp"\
	{$(INCLUDE)}"boost\preprocessor\cat.hpp"\
	{$(INCLUDE)}"boost\preprocessor\comma_if.hpp"\
	{$(INCLUDE)}"boost\preprocessor\config\config.hpp"\
	{$(INCLUDE)}"boost\preprocessor\control\detail\dmc\while.hpp"\
	{$(INCLUDE)}"boost\preprocessor\control\detail\edg\while.hpp"\
	{$(INCLUDE)}"boost\preprocessor\control\detail\msvc\while.hpp"\
	{$(INCLUDE)}"boost\preprocessor\control\detail\while.hpp"\
	{$(INCLUDE)}"boost\preprocessor\control\if.hpp"\
	{$(INCLUDE)}"boost\preprocessor\control\iif.hpp"\
	{$(INCLUDE)}"boost\preprocessor\control\while.hpp"\
	{$(INCLUDE)}"boost\preprocessor\debug\error.hpp"\
	{$(INCLUDE)}"boost\preprocessor\detail\auto_rec.hpp"\
	{$(INCLUDE)}"boost\preprocessor\detail\check.hpp"\
	{$(INCLUDE)}"boost\preprocessor\detail\dmc\auto_rec.hpp"\
	{$(INCLUDE)}"boost\preprocessor\detail\is_binary.hpp"\
	{$(INCLUDE)}"boost\preprocessor\empty.hpp"\
	{$(INCLUDE)}"boost\preprocessor\enum_params.hpp"\
	{$(INCLUDE)}"boost\preprocessor\facilities\empty.hpp"\
	{$(INCLUDE)}"boost\preprocessor\identity.hpp"\
	{$(INCLUDE)}"boost\preprocessor\inc.hpp"\
	{$(INCLUDE)}"boost\preprocessor\iterate.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\adt.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\append.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\detail\dmc\fold_left.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\detail\edg\fold_left.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\detail\edg\fold_right.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\detail\fold_left.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\detail\fold_right.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\fold_left.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\fold_right.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\for_each_i.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\reverse.hpp"\
	{$(INCLUDE)}"boost\preprocessor\list\transform.hpp"\
	{$(INCLUDE)}"boost\preprocessor\logical\and.hpp"\
	{$(INCLUDE)}"boost\preprocessor\logical\bitand.hpp"\
	{$(INCLUDE)}"boost\preprocessor\logical\bool.hpp"\
	{$(INCLUDE)}"boost\preprocessor\logical\compl.hpp"\
	{$(INCLUDE)}"boost\preprocessor\punctuation\comma.hpp"\
	{$(INCLUDE)}"boost\preprocessor\punctuation\comma_if.hpp"\
	{$(INCLUDE)}"boost\preprocessor\repeat.hpp"\
	{$(INCLUDE)}"boost\preprocessor\repetition\detail\dmc\for.hpp"\
	{$(INCLUDE)}"boost\preprocessor\repetition\detail\edg\for.hpp"\
	{$(INCLUDE)}"boost\preprocessor\repetition\detail\for.hpp"\
	{$(INCLUDE)}"boost\preprocessor\repetition\detail\msvc\for.hpp"\
	{$(INCLUDE)}"boost\preprocessor\repetition\enum_binary_params.hpp"\
	{$(INCLUDE)}"boost\preprocessor\repetition\enum_params.hpp"\
	{$(INCLUDE)}"boost\preprocessor\repetition\for.hpp"\
	{$(INCLUDE)}"boost\preprocessor\repetition\repeat.hpp"\
	{$(INCLUDE)}"boost\preprocessor\repetition\repeat_from_to.hpp"\
	{$(INCLUDE)}"boost\preprocessor\tuple\eat.hpp"\
	{$(INCLUDE)}"boost\preprocessor\tuple\elem.hpp"\
	{$(INCLUDE)}"boost\preprocessor\tuple\rem.hpp"\
	{$(INCLUDE)}"boost\preprocessor\tuple\to_list.hpp"\
	{$(INCLUDE)}"boost\regex.hpp"\
	{$(INCLUDE)}"boost\regex\config.hpp"\
	{$(INCLUDE)}"boost\regex\config\borland.hpp"\
	{$(INCLUDE)}"boost\regex\config\cwchar.hpp"\
	{$(INCLUDE)}"boost\regex\pattern_except.hpp"\
	{$(INCLUDE)}"boost\regex\pending\object_cache.hpp"\
	{$(INCLUDE)}"boost\regex\pending\static_mutex.hpp"\
	{$(INCLUDE)}"boost\regex\regex_traits.hpp"\
	{$(INCLUDE)}"boost\regex\v4\basic_regex.hpp"\
	{$(INCLUDE)}"boost\regex\v4\basic_regex_creator.hpp"\
	{$(INCLUDE)}"boost\regex\v4\basic_regex_parser.hpp"\
	{$(INCLUDE)}"boost\regex\v4\c_regex_traits.hpp"\
	{$(INCLUDE)}"boost\regex\v4\char_regex_traits.hpp"\
	{$(INCLUDE)}"boost\regex\v4\cpp_regex_traits.hpp"\
	{$(INCLUDE)}"boost\regex\v4\error_type.hpp"\
	{$(INCLUDE)}"boost\regex\v4\instances.hpp"\
	{$(INCLUDE)}"boost\regex\v4\iterator_category.hpp"\
	{$(INCLUDE)}"boost\regex\v4\iterator_traits.hpp"\
	{$(INCLUDE)}"boost\regex\v4\match_flags.hpp"\
	{$(INCLUDE)}"boost\regex\v4\match_results.hpp"\
	{$(INCLUDE)}"boost\regex\v4\perl_matcher.hpp"\
	{$(INCLUDE)}"boost\regex\v4\perl_matcher_common.hpp"\
	{$(INCLUDE)}"boost\regex\v4\perl_matcher_non_recursive.hpp"\
	{$(INCLUDE)}"boost\regex\v4\perl_matcher_recursive.hpp"\
	{$(INCLUDE)}"boost\regex\v4\primary_transform.hpp"\
	{$(INCLUDE)}"boost\regex\v4\protected_call.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regbase.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_format.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_fwd.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_grep.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_iterator.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_match.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_merge.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_raw_buffer.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_replace.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_search.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_split.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_token_iterator.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_traits.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_traits_defaults.hpp"\
	{$(INCLUDE)}"boost\regex\v4\regex_workaround.hpp"\
	{$(INCLUDE)}"boost\regex\v4\states.hpp"\
	{$(INCLUDE)}"boost\regex\v4\sub_match.hpp"\
	{$(INCLUDE)}"boost\regex\v4\syntax_type.hpp"\
	{$(INCLUDE)}"boost\regex\v4\w32_regex_traits.hpp"\
	{$(INCLUDE)}"boost\regex_fwd.hpp"\
	{$(INCLUDE)}"boost\scoped_array.hpp"\
	{$(INCLUDE)}"boost\scoped_ptr.hpp"\
	{$(INCLUDE)}"boost\shared_ptr.hpp"\
	{$(INCLUDE)}"boost\static_assert.hpp"\
	{$(INCLUDE)}"boost\thread\detail\config.hpp"\
	{$(INCLUDE)}"boost\thread\detail\lock.hpp"\
	{$(INCLUDE)}"boost\thread\exceptions.hpp"\
	{$(INCLUDE)}"boost\thread\once.hpp"\
	{$(INCLUDE)}"boost\thread\recursive_mutex.hpp"\
	{$(INCLUDE)}"boost\throw_exception.hpp"\
	{$(INCLUDE)}"boost\type.hpp"\
	{$(INCLUDE)}"boost\type_traits\add_const.hpp"\
	{$(INCLUDE)}"boost\type_traits\add_reference.hpp"\
	{$(INCLUDE)}"boost\type_traits\alignment_of.hpp"\
	{$(INCLUDE)}"boost\type_traits\broken_compiler_spec.hpp"\
	{$(INCLUDE)}"boost\type_traits\config.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\bool_trait_def.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\bool_trait_undef.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\cv_traits_impl.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\false_result.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\ice_and.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\ice_eq.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\ice_not.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\ice_or.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\is_function_ptr_helper.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\is_function_ptr_tester.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\is_mem_fun_pointer_impl.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\is_mem_fun_pointer_tester.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\size_t_trait_def.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\size_t_trait_undef.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\template_arity_spec.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\type_trait_def.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\type_trait_undef.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\wrap.hpp"\
	{$(INCLUDE)}"boost\type_traits\detail\yes_no_type.hpp"\
	{$(INCLUDE)}"boost\type_traits\has_trivial_assign.hpp"\
	{$(INCLUDE)}"boost\type_traits\has_trivial_constructor.hpp"\
	{$(INCLUDE)}"boost\type_traits\has_trivial_copy.hpp"\
	{$(INCLUDE)}"boost\type_traits\has_trivial_destructor.hpp"\
	{$(INCLUDE)}"boost\type_traits\ice.hpp"\
	{$(INCLUDE)}"boost\type_traits\integral_constant.hpp"\
	{$(INCLUDE)}"boost\type_traits\intrinsics.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_abstract.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_arithmetic.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_array.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_class.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_const.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_convertible.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_enum.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_float.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_function.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_integral.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_member_function_pointer.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_member_pointer.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_pod.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_pointer.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_polymorphic.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_reference.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_same.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_scalar.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_union.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_void.hpp"\
	{$(INCLUDE)}"boost\type_traits\is_volatile.hpp"\
	{$(INCLUDE)}"boost\type_traits\msvc\remove_bounds.hpp"\
	{$(INCLUDE)}"boost\type_traits\msvc\remove_cv.hpp"\
	{$(INCLUDE)}"boost\type_traits\msvc\remove_reference.hpp"\
	{$(INCLUDE)}"boost\type_traits\msvc\typeof.hpp"\
	{$(INCLUDE)}"boost\type_traits\remove_bounds.hpp"\
	{$(INCLUDE)}"boost\type_traits\remove_cv.hpp"\
	{$(INCLUDE)}"boost\type_traits\remove_reference.hpp"\
	{$(INCLUDE)}"boost\type_traits\type_with_alignment.hpp"\
	{$(INCLUDE)}"boost\utility.hpp"\
	{$(INCLUDE)}"boost\utility\addressof.hpp"\
	{$(INCLUDE)}"boost\utility\base_from_member.hpp"\
	{$(INCLUDE)}"boost\utility\enable_if.hpp"\
	{$(INCLUDE)}"boost\version.hpp"\
	{$(INCLUDE)}"mem.h"\
	{$(INCLUDE)}"pthread.h"\
	{$(INCLUDE)}"stl\_abbrevs.h"\
	{$(INCLUDE)}"stl\_algobase.c"\
	{$(INCLUDE)}"stl\_algobase.h"\
	{$(INCLUDE)}"stl\_alloc.c"\
	{$(INCLUDE)}"stl\_alloc.h"\
	{$(INCLUDE)}"stl\_cmath.h"\
	{$(INCLUDE)}"stl\_config_compat_post.h"\
	{$(INCLUDE)}"stl\_construct.h"\
	{$(INCLUDE)}"stl\_cprolog.h"\
	{$(INCLUDE)}"stl\_cstdarg.h"\
	{$(INCLUDE)}"stl\_cstddef.h"\
	{$(INCLUDE)}"stl\_cstdio.h"\
	{$(INCLUDE)}"stl\_cstdlib.h"\
	{$(INCLUDE)}"stl\_cstring.h"\
	{$(INCLUDE)}"stl\_ctime.h"\
	{$(INCLUDE)}"stl\_ctraits_fns.h"\
	{$(INCLUDE)}"stl\_cwchar.h"\
	{$(INCLUDE)}"stl\_epilog.h"\
	{$(INCLUDE)}"stl\_exception.h"\
	{$(INCLUDE)}"stl\_function.h"\
	{$(INCLUDE)}"stl\_function_adaptors.h"\
	{$(INCLUDE)}"stl\_function_base.h"\
	{$(INCLUDE)}"stl\_iosfwd.h"\
	{$(INCLUDE)}"stl\_iterator.h"\
	{$(INCLUDE)}"stl\_iterator_base.h"\
	{$(INCLUDE)}"stl\_iterator_old.h"\
	{$(INCLUDE)}"stl\_mbstate_t.h"\
	{$(INCLUDE)}"stl\_move_construct_fwk.h"\
	{$(INCLUDE)}"stl\_new.h"\
	{$(INCLUDE)}"stl\_pair.h"\
	{$(INCLUDE)}"stl\_prolog.h"\
	{$(INCLUDE)}"stl\_pthread_alloc.h"\
	{$(INCLUDE)}"stl\_ptrs_specialize.h"\
	{$(INCLUDE)}"stl\_range_errors.c"\
	{$(INCLUDE)}"stl\_range_errors.h"\
	{$(INCLUDE)}"stl\_sparc_atomic.h"\
	{$(INCLUDE)}"stl\_stdexcept.h"\
	{$(INCLUDE)}"stl\_stdexcept_base.c"\
	{$(INCLUDE)}"stl\_stdexcept_base.h"\
	{$(INCLUDE)}"stl\_stlport_version.h"\
	{$(INCLUDE)}"stl\_string.c"\
	{$(INCLUDE)}"stl\_string.h"\
	{$(INCLUDE)}"stl\_string_base.h"\
	{$(INCLUDE)}"stl\_string_fwd.h"\
	{$(INCLUDE)}"stl\_string_npos.h"\
	{$(INCLUDE)}"stl\_string_operators.h"\
	{$(INCLUDE)}"stl\_string_sum.h"\
	{$(INCLUDE)}"stl\_string_sum_methods.h"\
	{$(INCLUDE)}"stl\_string_workaround.h"\
	{$(INCLUDE)}"stl\_threads.c"\
	{$(INCLUDE)}"stl\_threads.h"\
	{$(INCLUDE)}"stl\_uninitialized.h"\
	{$(INCLUDE)}"stl\boost_type_traits.h"\
	{$(INCLUDE)}"stl\char_traits.h"\
	{$(INCLUDE)}"stl\config\_aix.h"\
	{$(INCLUDE)}"stl\config\_apcc.h"\
	{$(INCLUDE)}"stl\config\_as400.h"\
	{$(INCLUDE)}"stl\config\_auto_link.h"\
	{$(INCLUDE)}"stl\config\_bc.h"\
	{$(INCLUDE)}"stl\config\_como.h"\
	{$(INCLUDE)}"stl\config\_cygwin.h"\
	{$(INCLUDE)}"stl\config\_dec.h"\
	{$(INCLUDE)}"stl\config\_dec_vms.h"\
	{$(INCLUDE)}"stl\config\_detect_dll_or_lib.h"\
	{$(INCLUDE)}"stl\config\_dm.h"\
	{$(INCLUDE)}"stl\config\_epilog.h"\
	{$(INCLUDE)}"stl\config\_evc.h"\
	{$(INCLUDE)}"stl\config\_feedback.h"\
	{$(INCLUDE)}"stl\config\_freebsd.h"\
	{$(INCLUDE)}"stl\config\_fujitsu.h"\
	{$(INCLUDE)}"stl\config\_gcc.h"\
	{$(INCLUDE)}"stl\config\_hpacc.h"\
	{$(INCLUDE)}"stl\config\_hpux.h"\
	{$(INCLUDE)}"stl\config\_ibm.h"\
	{$(INCLUDE)}"stl\config\_icc.h"\
	{$(INCLUDE)}"stl\config\_intel.h"\
	{$(INCLUDE)}"stl\config\_kai.h"\
	{$(INCLUDE)}"stl\config\_linux.h"\
	{$(INCLUDE)}"stl\config\_mac.h"\
	{$(INCLUDE)}"stl\config\_macosx.h"\
	{$(INCLUDE)}"stl\config\_msvc.h"\
	{$(INCLUDE)}"stl\config\_mwerks.h"\
	{$(INCLUDE)}"stl\config\_native_headers.h"\
	{$(INCLUDE)}"stl\config\_openbsd.h"\
	{$(INCLUDE)}"stl\config\_prolog.h"\
	{$(INCLUDE)}"stl\config\_sgi.h"\
	{$(INCLUDE)}"stl\config\_solaris.h"\
	{$(INCLUDE)}"stl\config\_sunprocc.h"\
	{$(INCLUDE)}"stl\config\_system.h"\
	{$(INCLUDE)}"stl\config\_warnings_off.h"\
	{$(INCLUDE)}"stl\config\_watcom.h"\
	{$(INCLUDE)}"stl\config\_windows.h"\
	{$(INCLUDE)}"stl\config\compat.h"\
	{$(INCLUDE)}"stl\config\features.h"\
	{$(INCLUDE)}"stl\config\host.h"\
	{$(INCLUDE)}"stl\config\stl_confix.h"\
	{$(INCLUDE)}"stl\config\stl_mycomp.h"\
	{$(INCLUDE)}"stl\config\user_config.h"\
	{$(INCLUDE)}"stl\debug\_debug.c"\
	{$(INCLUDE)}"stl\debug\_debug.h"\
	{$(INCLUDE)}"stl\debug\_iterator.h"\
	{$(INCLUDE)}"stl\debug\_string.h"\
	{$(INCLUDE)}"stl\debug\_string_sum_methods.h"\
	{$(INCLUDE)}"stl\msl_string.h"\
	{$(INCLUDE)}"stl\type_manips.h"\
	{$(INCLUDE)}"stl\type_traits.h"\
	{$(INCLUDE)}"using\cstring"\
	
NODEP_CPP_EKBED=\
	"..\..\usr\include\pthread.h"\
	"..\..\usr\include\sys\cdefs.h"\
	"D:\bhj\boost_1_34_1\boost\thread\scoped_critical_region.hpp"\
	
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
