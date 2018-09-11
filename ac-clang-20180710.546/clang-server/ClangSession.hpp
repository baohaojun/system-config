/* -*- mode: c++ ; coding: utf-8-unix -*- */
/*  last updated : 2018/05/14.19:33:10 */

/*
 * Copyright (c) 2013-2018 yaruopooner [https://github.com/yaruopooner]
 *
 * This file is part of clang-server.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


#pragma once

#ifndef __CLANG_SESSION_HPP__
#define __CLANG_SESSION_HPP__




/*================================================================================================*/
/*  Include Files                                                                                 */
/*================================================================================================*/

#include "Common.hpp"
#include "Command.hpp"


/*================================================================================================*/
/*  Global Class Method Definitions Section                                                       */
/*================================================================================================*/



class ClangSession
{
public:
    ClangSession( const std::string& inSessionName, const ClangContext& inClangContext, CommandContext& ioCommandContext );
    virtual ~ClangSession( void );

    void Allocate( void );
    void Deallocate( void );


    // const CFlagsBuffer& GetCFlagsBuffer( void ) const
    // {
    //  return ( m_CFlagsBuffer );
    // }
    // CFlagsBuffer& GetCFlagsBuffer( void )
    // {
    //  return ( m_CFlagsBuffer );
    // }


    // commands
    void commandSuspend( void );
    void commandResume( void );
    void commandSetCFlags( void );
    void commandSetSourceCode( void );
    void commandReparse( void );
    void commandCompletion( void );
    void commandDiagnostics( void );
    void commandInclusion( void );
    void commandDeclaration( void );
    void commandDefinition( void );
    void commandSmartJump( void );


private:    
    CXUnsavedFile GetCXUnsavedFile( void ) const
    {
        CXUnsavedFile           unsaved_file;

        unsaved_file.Filename = m_SessionName.c_str();
        unsaved_file.Contents = m_CSourceCodeBuffer.GetAddress< const char* >();
        unsaved_file.Length   = static_cast< uint32_t >( m_CSourceCodeBuffer.GetSize() );

        return unsaved_file;
    }

    void CreateTranslationUnit( void );
    void DeleteTranslationUnit( void );


// private:
    // internal command classes
    struct Command
    {
        class ReadCFlags;
        class ReadSourceCode;
        class ReadLineColumn;
        class Completion;
        class Diagnostics;
        class Jump;
    };


private:    
    const std::string   m_SessionName;
    const ClangContext& m_ClangContext;
    CommandContext&     m_CommandContext;

    // clang parser object
    CXTranslationUnit   m_CxTU;

    // clang parser options
    uint32_t            m_TranslationUnitFlags;
    uint32_t            m_CompleteAtFlags;

    CFlagsBuffer        m_CFlagsBuffer;
    // CSourceCodeBuffer   m_CSourceCodeBuffer;
    Buffer              m_CSourceCodeBuffer;
    uint32_t            m_Line;
    uint32_t            m_Column;
};




#endif  // __CLANG_SESSION_HPP__
/*================================================================================================*/
/*  EOF                                                                                           */
/*================================================================================================*/
