/* -*- mode: c++ ; coding: utf-8-unix -*- */
/*  last updated : 2015/09/05.04:13:59 */

/*
 * Copyright (c) 2013-2015 yaruopooner [https://github.com/yaruopooner]
 *
 * This file is part of ac-clang.
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

#ifndef __CLANG_SERVER_HPP__
#define __CLANG_SERVER_HPP__



#define CLANG_SERVER_VERSION       "server version 1.4.0"


/*================================================================================================*/
/*  Comment                                                                                       */
/*================================================================================================*/


/*================================================================================================*/
/*  Include Files                                                                                 */
/*================================================================================================*/

#include <memory>
#include <functional>
#include <unordered_map>

#include "ClangSession.hpp"


/*================================================================================================*/
/*  Class                                                                                         */
/*================================================================================================*/


class   ClangServer
{
public:
    enum Status
    {
        kStatus_Running, 
        kStatus_Exit, 
    };


    struct Specification
    {
        enum
        {
            kStreamBuffer_UnitSize = 1 * 1024 * 1024, 
        };
    
        Specification( size_t StdinBufferSize = kStreamBuffer_UnitSize,
                       size_t StdoutBufferSize = kStreamBuffer_UnitSize,
                       const std::string& LogFile = std::string() ) : 
            m_StdinBufferSize( StdinBufferSize )
            , m_StdoutBufferSize( StdoutBufferSize )
            , m_LogFile( LogFile )
        {
        }

        size_t      m_StdinBufferSize;
        size_t      m_StdoutBufferSize;
        std::string m_LogFile;
    };


    ClangServer( const Specification& specification = Specification() );
    ~ClangServer( void );

    void    ParseCommand( void );

    // void    SetLogFile( const std::string& LogFile );
    

private:    
    void    ParseServerCommand( void );
    void    ParseSessionCommand( void );


    // commands
    void    commandGetSpecification( void );
    void    commandGetClangVersion( void );
    void    commandSetClangParameters( void );
    void    commandCreateSession( void );
    void    commandDeleteSession( void );
    void    commandReset( void );
    void    commandShutdown( void );


private:
    typedef std::unordered_map< std::string, std::function< void (ClangServer&) > >     ServerHandleMap;
    typedef std::unordered_map< std::string, std::function< void (ClangSession&) > >    SessionHandleMap;
    typedef std::unordered_map< std::string, std::shared_ptr< ClangSession > >          Dictionary;


    ClangContext        m_Context;
    ServerHandleMap     m_ServerCommands;
    SessionHandleMap    m_SessionCommands;
    Dictionary          m_Sessions;
    StreamReader        m_Reader;
    StreamWriter        m_Writer;
    uint32_t            m_Status;
    Specification       m_Specification;
};




#endif  // __CLANG_SERVER_HPP__
/*================================================================================================*/
/*  EOF                                                                                           */
/*================================================================================================*/
