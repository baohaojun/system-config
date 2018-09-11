/* -*- mode: c++ ; coding: utf-8-unix -*- */
/*  last updated : 2018/05/14.19:33:08 */

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


/*================================================================================================*/
/*  Comment                                                                                       */
/*================================================================================================*/


/*================================================================================================*/
/*  Include Files                                                                                 */
/*================================================================================================*/

#include "Profiler.hpp"
#include "ClangServer.hpp"




/*================================================================================================*/
/*  Internal Function Definitions Section                                                         */
/*================================================================================================*/

namespace
{

static std::string sGetClangVersion( void )
{
    CXString            version_text  = clang_getClangVersion();
    const std::string   clang_version = clang_getCString( version_text );

    clang_disposeString( version_text );

    return clang_version;
}

}



/*================================================================================================*/
/*  Global Class Method Definitions Section                                                       */
/*================================================================================================*/


/*

   ---- SERVER MESSAGE ---- 

   - GET_SPECIFIC_PROPERTIES: return clang-server specific properties
     Message format:
        command_type:Server
        command_name:GET_SPECIFIC_PROPERTIES

   - GET_CLANG_VERSION: return libclang version (libclang.lib/a)
     Message format:
        command_type:Server
        command_name:GET_CLANG_VERSION

   - SET_CLANG_PARAMETERS: setup libclang behavior parameters
     Message format:
        command_type:Server
        command_name:SET_CLANG_PARAMETERS
        translation_unit_flags:[#flag_name0|flag_name1|...#]
        complete_at_flags:[#flag_name0|flag_name1|...#]

   - CREATE_SESSION: create session.
     Message format:
        command_type:Server
        command_name:CREATE_SESSION
        session_name::[#session_name#]

   - DELETE_SESSION: delete session.
     Message format:
        command_type:Server
        command_name:DELETE_SESSION
        session_name::[#session_name#]

   - RESET: reset the clang server(all session delete. context reallocate)
     Message format:
        command_type:Server
        command_name:RESET

   - SHUTDOWN: shutdown the clang server (this program)
     Message format:
        command_type:Server
        command_name:SHUTDOWN


   ---- SESSION MESSAGE ----

   - SUSPEND: delete CXTranslationUnit
     Message format: 
        command_type:Session
        command_name:SUSPEND
        session_name:[#session_name#]
     
   - RESUME: realloc CXTranslationUnit
     Message format: 
        command_type:Session
        command_name:RESUME
        session_name:[#session_name#]
     
   - SET_CFLAGS: Specify CFLAGS argument passing to clang parser.
     Message format:
        command_type:Session
        command_name:SET_CFLAGS
        session_name:[#session_name#]
        num_cflags:[#num_cflags#]
            arg1 arg2 ...... (there should be num_cflags items here)
            CFLAGS's white space must be accept.
            a separator is '\n'.
        source_length:[#source_length#]
        <# SOURCE CODE #>

   - SET_SOURCECODE: Update the source code in the source buffer
     Message format:
        command_type:Session
        command_name:SET_SOURCECODE
        session_name:[#session_name#]
        source_length:[#source_length#]
        <# SOURCE CODE #>

   - REPARSE: Reparse the source code
        command_type:Session
        command_name:REPARSE
        session_name:[#session_name#]

   - COMPLETION: Do code completion at a specified point.
     Message format: 
        command_type:Session
        command_name:COMPLETION
        session_name:[#session_name#]
        line:[#line#]
        column:[#column#]
        source_length:[#source_length#]
        <# SOURCE CODE #>

   - SYNTAXCHECK: Retrieve diagnostic messages
     Message format:
        command_type:Session
        command_name:SYNTAXCHECK
        session_name:[#session_name#]
        source_length:[#source_length#]
        <# SOURCE CODE #>

   - INCLUSION: Get location of inclusion file at point 
     Message format: 
        command_type:Session
        command_name:INCLUSION
        session_name:[#session_name#]
        line:[#line#]
        column:[#column#]
        source_length:[#source_length#]
        <# SOURCE CODE #>

   - DEFINITION: Get location of definition at point 
     Message format: 
        command_type:Session
        command_name:DEFINITION
        session_name:[#session_name#]
        line:[#line#]
        column:[#column#]
        source_length:[#source_length#]
        <# SOURCE CODE #>

   - DECLARATION: Get location of declaration at point 
     Message format: 
        command_type:Session
        command_name:DECLARATION
        session_name:[#session_name#]
        line:[#line#]
        column:[#column#]
        source_length:[#source_length#]
        <# SOURCE CODE #>

   - SMARTJUMP: Get location of inclusion, definition, declaration at point. If that fails, it will search following. 
                inclusion -> finish, definition -> declaration -> finish.
     Message format: 
        command_type:Session
        command_name:SMARTJUMP
        session_name:[#session_name#]
        line:[#line#]
        column:[#column#]
        source_length:[#source_length#]
        <# SOURCE CODE #>


*/




ClangServer::ClangServer( const Specification& inSpecification ) : 
    m_Status( kStatus_Running )
    , m_Specification( inSpecification )
{
    // setup stream buffer size
    m_Specification.m_StdinBufferSize  = std::max( m_Specification.m_StdinBufferSize, static_cast< size_t >( Specification::kStreamBuffer_UnitSize ) );
    m_Specification.m_StdoutBufferSize = std::max( m_Specification.m_StdoutBufferSize, static_cast< size_t >( Specification::kStreamBuffer_UnitSize ) );

    std::setvbuf( stdin, nullptr, _IOFBF, m_Specification.m_StdinBufferSize );
    std::setvbuf( stdout, nullptr, _IOFBF, m_Specification.m_StdoutBufferSize );


    // command context
    {
        IDataObject::EType  input_allocate_type  = IDataObject::EType::kLispNode;
        IDataObject::EType  output_allocate_type = IDataObject::EType::kLispText;

        if ( inSpecification.m_InputDataType == EIoDataType::kJson )
        {
            input_allocate_type = IDataObject::EType::kJson;
        }
        if ( inSpecification.m_OutputDataType == EIoDataType::kJson )
        {
            output_allocate_type = IDataObject::EType::kJson;
        }

        m_CommandContext.AllocateDataObject( input_allocate_type, output_allocate_type );
    }


    // server command
    m_ServerCommands.insert( ServerHandleMap::value_type( "GET_SPECIFICATION", std::mem_fn( &ClangServer::commandGetSpecification ) ) );
    m_ServerCommands.insert( ServerHandleMap::value_type( "GET_CLANG_VERSION", std::mem_fn( &ClangServer::commandGetClangVersion ) ) );
    m_ServerCommands.insert( ServerHandleMap::value_type( "SET_CLANG_PARAMETERS", std::mem_fn( &ClangServer::commandSetClangParameters ) ) );
    m_ServerCommands.insert( ServerHandleMap::value_type( "CREATE_SESSION", std::mem_fn( &ClangServer::commandCreateSession ) ) );
    m_ServerCommands.insert( ServerHandleMap::value_type( "DELETE_SESSION", std::mem_fn( &ClangServer::commandDeleteSession ) ) );
    m_ServerCommands.insert( ServerHandleMap::value_type( "RESET", std::mem_fn( &ClangServer::commandReset ) ) );
    m_ServerCommands.insert( ServerHandleMap::value_type( "SHUTDOWN", std::mem_fn( &ClangServer::commandShutdown ) ) );

    // session command
    m_SessionCommands.insert( SessionHandleMap::value_type( "SUSPEND", std::mem_fn( &ClangSession::commandSuspend ) ) );
    m_SessionCommands.insert( SessionHandleMap::value_type( "RESUME", std::mem_fn( &ClangSession::commandResume ) ) );
    m_SessionCommands.insert( SessionHandleMap::value_type( "SET_CFLAGS", std::mem_fn( &ClangSession::commandSetCFlags ) ) );
    m_SessionCommands.insert( SessionHandleMap::value_type( "SET_SOURCECODE", std::mem_fn( &ClangSession::commandSetSourceCode ) ) );
    m_SessionCommands.insert( SessionHandleMap::value_type( "REPARSE", std::mem_fn( &ClangSession::commandReparse ) ) );
    m_SessionCommands.insert( SessionHandleMap::value_type( "COMPLETION", std::mem_fn( &ClangSession::commandCompletion ) ) );
    m_SessionCommands.insert( SessionHandleMap::value_type( "SYNTAXCHECK", std::mem_fn( &ClangSession::commandDiagnostics ) ) );
    m_SessionCommands.insert( SessionHandleMap::value_type( "INCLUSION", std::mem_fn( &ClangSession::commandInclusion ) ) );
    m_SessionCommands.insert( SessionHandleMap::value_type( "DEFINITION", std::mem_fn( &ClangSession::commandDefinition ) ) );
    m_SessionCommands.insert( SessionHandleMap::value_type( "DECLARATION", std::mem_fn( &ClangSession::commandDeclaration ) ) );
    m_SessionCommands.insert( SessionHandleMap::value_type( "SMARTJUMP", std::mem_fn( &ClangSession::commandSmartJump ) ) );

    // display initial specification
    // commandGetSpecification();
}

ClangServer::~ClangServer( void )
{
    // m_Sessions must be destruction early than m_ClangContext.
    // Because m_Sessions depend to m_ClangContext.
    m_Sessions.clear();
}



class ClangServer::Command::GetSpecification : public ICommand
{
public:
    GetSpecification( ClangServer& inServer ) :
        m_Server( inServer )
    {
    }

    std::string GetIoDataTypeString( EIoDataType inDataType ) const
    {
        return ( inDataType == EIoDataType::kJson ) ? "json" : "s-expression";
    }

    virtual void Write( Lisp::Text::Object& outData ) const override
    {
        const std::string   server_version   = CLANG_SERVER_VERSION;
        const std::string   clang_version    = ::sGetClangVersion();
        const std::string   generate         = CMAKE_GENERATOR "/" CMAKE_HOST_SYSTEM_PROCESSOR;
        const std::string   input_data_type  = GetIoDataTypeString( m_Server.m_Specification.m_InputDataType );
        const std::string   output_data_type = GetIoDataTypeString( m_Server.m_Specification.m_OutputDataType );

        Lisp::Text::NewList plist( outData );

        plist.AddProperty( ":RequestId", m_Server.m_CommandContext.GetRequestId() );
        plist.AddSymbol( ":Results" );

        {
            Lisp::Text::NewList results_plist( plist );

            results_plist.AddProperty( ":ServerVersion", server_version );
            results_plist.AddProperty( ":ClangVersion", clang_version );
            results_plist.AddProperty( ":Generate", generate );
            results_plist.AddProperty( ":StdinBufferSize", m_Server.m_Specification.m_StdinBufferSize );
            results_plist.AddProperty( ":StdoutBufferSize", m_Server.m_Specification.m_StdoutBufferSize );
            results_plist.AddProperty( ":InputDataType", input_data_type );
            results_plist.AddProperty( ":OutputDataType", output_data_type );
        }
    }
    virtual void Write( Json& outData ) const override
    {
        const std::string   server_version   = CLANG_SERVER_VERSION;
        const std::string   clang_version    = ::sGetClangVersion();
        const std::string   generate         = CMAKE_GENERATOR "/" CMAKE_HOST_SYSTEM_PROCESSOR;
        const std::string   input_data_type  = GetIoDataTypeString( m_Server.m_Specification.m_InputDataType );
        const std::string   output_data_type = GetIoDataTypeString( m_Server.m_Specification.m_OutputDataType );

        outData[ "RequestId" ] = m_Server.m_CommandContext.GetRequestId();
        outData[ "Results" ]   = 
            {
                { "ServerVersion", server_version },
                { "ClangVersion", clang_version },
                { "Generate", generate },
                { "StdinBufferSize", m_Server.m_Specification.m_StdinBufferSize },
                { "StdoutBufferSize", m_Server.m_Specification.m_StdoutBufferSize },
                { "InputDataType", input_data_type },
                { "OutputDataType", output_data_type },
            };
    }

private:
    // input
    ClangServer&               m_Server;
};


class ClangServer::Command::GetClangVersion : public ICommand
{
public:
    GetClangVersion( ClangServer& inServer ) :
        m_Server( inServer )
    {
    }

    virtual void Write( Lisp::Text::Object& outData ) const override
    {
        const std::string   clang_version = ::sGetClangVersion();

        Lisp::Text::NewList plist( outData );

        plist.AddProperty( ":RequestId", m_Server.m_CommandContext.GetRequestId() );
        plist.AddSymbol( ":Results" );

        {
            Lisp::Text::NewList results_plist( plist );

            results_plist.AddProperty( ":ClangVersion", clang_version );
        }
    }
    virtual void Write( Json& outData ) const override
    {
        const std::string   clang_version = ::sGetClangVersion();

        outData[ "RequestId" ] = m_Server.m_CommandContext.GetRequestId();
        outData[ "Results" ]   = 
            {
                { "ClangVersion", clang_version },
            };
    }

private:
    // input
    ClangServer&               m_Server;
};


class ClangServer::Command::SetClangParameters : public ICommand
{
public:
    SetClangParameters( ClangServer& inServer ) :
        m_Server( inServer )
    {
    }

    virtual bool Evaluate( void ) override
    {
        const uint32_t  translation_unit_flags_value = ClangFlagConverters::sGetCXTranslationUnitFlags().GetValue( m_TranslationUnitFlags );
        const uint32_t  complete_at_flags_value      = ClangFlagConverters::sGetCXCodeCompleteFlags().GetValue( m_CompleteAtFlags );

        m_Server.m_ClangContext.SetTranslationUnitFlags( translation_unit_flags_value );
        m_Server.m_ClangContext.SetCompleteAtFlags( complete_at_flags_value );
        m_Server.m_ClangContext.SetCompleteResultsLimit( m_CompleteResultsLimit );

        return true;
    }

    virtual void Read( const Lisp::Text::Object& inData ) override
    {
        Lisp::SAS::DetectHandler    handler;
        Lisp::SAS::Parser           parser;
        uint32_t                    read_count = 0;

        handler.m_OnEnterSequence = [this]( Lisp::SAS::DetectHandler::SequenceContext& ioContext ) -> bool
            {
                ioContext.m_Mode = Lisp::SAS::DetectHandler::SequenceContext::ParseMode::kPropertyList;

                return true;
            };
        handler.m_OnProperty = [this, &read_count]( const size_t inIndex, const std::string& inSymbol, const Lisp::SAS::SExpression& inSExpression ) -> bool
            {
                if ( inSymbol == ":TranslationUnitFlags" )
                {
                    m_TranslationUnitFlags = inSExpression.GetValue< std::string >();
                    ++read_count;
                }
                else if ( inSymbol == ":CompleteAtFlags" )
                {
                    m_CompleteAtFlags = inSExpression.GetValue< std::string >();
                    ++read_count;
                }
                else if ( inSymbol == ":CompleteResultsLimit" )
                {
                    m_CompleteResultsLimit = inSExpression.GetValue< uint32_t >();
                    ++read_count;
                }

                if ( read_count == 3 )
                {
                    return false;
                }

                return true;
            };

        parser.Parse( inData, handler );
    }
    virtual void Read( const Lisp::Node::Object& inData ) override
    {
        // RequestId, command-type, command-name, session-name, is-profile
        Lisp::Node::PropertyListIterator    iterator = inData.GetRootPropertyListIterator();

        for ( ; !iterator.IsEnd(); iterator.Next() )
        {
            if ( iterator.IsSameKey( ":TranslationUnitFlags" ) )
            {
                m_TranslationUnitFlags = iterator.GetValue< std::string >();
            }
            else if ( iterator.IsSameKey( ":CompleteAtFlags" ) )
            {
                m_CompleteAtFlags = iterator.GetValue< std::string >();
            }
            else if ( iterator.IsSameKey( ":CompleteResultsLimit" ) )
            {
                m_CompleteResultsLimit = iterator.GetValue< int32_t >();
            }
        }
    }
    virtual void Read( const Json& inData ) override
    {
        m_TranslationUnitFlags = inData[ "TranslationUnitFlags" ];
        m_CompleteAtFlags      = inData[ "CompleteAtFlags" ];
        m_CompleteResultsLimit = inData[ "CompleteResultsLimit" ];
    }

private:
    // input
    ClangServer&    m_Server;
    std::string     m_TranslationUnitFlags;
    std::string     m_CompleteAtFlags;
    uint32_t        m_CompleteResultsLimit;
};



void ClangServer::commandGetSpecification( void )
{
    CommandEvaluator< Command::GetSpecification >   command( *this, m_CommandContext );
}


void ClangServer::commandGetClangVersion( void )
{
    CommandEvaluator< Command::GetClangVersion >    command( *this, m_CommandContext );
}


void ClangServer::commandSetClangParameters( void )
{
    CommandEvaluator< Command::SetClangParameters > command( *this, m_CommandContext );
}


void ClangServer::commandCreateSession( void )
{
    const std::string&      session_name = m_CommandContext.GetSessionName();

    // search session
    Dictionary::iterator    session_it   = m_Sessions.find( session_name );

    if ( session_it == m_Sessions.end() )
    {
        // not found session
        // allocate & setup new session
        std::shared_ptr< ClangSession >         new_session( std::make_shared< ClangSession >( session_name, m_ClangContext, m_CommandContext ) );
        std::pair< Dictionary::iterator, bool > result = m_Sessions.insert( Dictionary::value_type( session_name, new_session ) );

        if ( result.second )
        {
            // success
            new_session->Allocate();
        }
    }
    else 
    {
        // already exist
    }
}


void ClangServer::commandDeleteSession( void )
{
    const std::string&      session_name = m_CommandContext.GetSessionName();

    // search session
    Dictionary::iterator    session_it   = m_Sessions.find( session_name );

    if ( session_it != m_Sessions.end() )
    {
        // session_it->second->Deallocate();
        
        m_Sessions.erase( session_it );
    }
}


void ClangServer::commandReset( void )
{
    m_Sessions.clear();
    m_ClangContext.Deallocate();
    m_ClangContext.Allocate();
}


void ClangServer::commandShutdown( void )
{
    m_Status = kStatus_Exit;
}




void ClangServer::ParseServerCommand( void )
{
    const std::string&          command_name = m_CommandContext.GetCommandName();
    ServerHandleMap::iterator   command_it   = m_ServerCommands.find( command_name );
    PROFILER_SCOPED_SAMPLE_FUNCTION();

    // execute command handler
    if ( command_it != m_ServerCommands.end() )
    {
        command_it->second( *this );
    }
    else
    {
        // unknown command
    }
}

void ClangServer::ParseSessionCommand( void )
{
    const std::string&  command_name = m_CommandContext.GetCommandName();
    const std::string&  session_name = m_CommandContext.GetSessionName();

    if ( session_name.empty() )
    {
        return;
    }

    // search session
    Dictionary::iterator    session_it = m_Sessions.find( session_name );

    // execute command handler
    if ( session_it != m_Sessions.end() )
    {
        SessionHandleMap::iterator      command_it = m_SessionCommands.find( command_name );
        PROFILER_SCOPED_SAMPLE_FUNCTION();

        if ( command_it != m_SessionCommands.end() )
        {
            command_it->second( *session_it->second );
        }
        else
        {
            // session not found
        }
    }
    else
    {
        // unknown command
    }
}


void ClangServer::ParseCommand( void )
{
    PacketManager   packet_manager;
    const Buffer&   receive_buffer = packet_manager.GetReceiveBuffer();
    Buffer&         send_buffer    = packet_manager.GetSendBuffer();

    do
    {
        // Packet Receive
        packet_manager.Receive();
        {
            PROFILER_SCOPED_SAMPLE( "Packet Decode" );
            // receive packet to DataObject
            m_CommandContext.SetInputData( receive_buffer.GetAddress() );
        }
        
        // Command Transaction
        const std::string&    command_type = m_CommandContext.GetCommandType();

        if ( command_type == "Server" )
        {
            ParseServerCommand();
        }
        else if ( command_type == "Session" )
        {
            ParseSessionCommand();
        }
        else
        {
            // unknown command type
        }

        // Profile
        if ( m_CommandContext.IsProfile() )
        {
            IDataObject*  data_object = m_CommandContext.GetOutputDataObject();

            data_object->Encode( m_CommandContext );
        }


        // Packet Send
        PROFILER_SAMPLE_BEGIN( profile0, "Packet Generate" );

        // packet generation from DataObject
        IDataObject*        data_object   = m_CommandContext.GetOutputDataObject();
        const std::string   export_string = data_object->ToString();

        PROFILER_SAMPLE_END( profile0 );

        if ( !export_string.empty() )
        {
            PROFILER_SCOPED_SAMPLE( "Packet Send" );
            // command success

            // NOTICE:
            // the cost of copying is useless. optimaization in necessary.
            // it is better to pass export_string by reference to send.
            // send_buffer.Allocate( export_string.size() + 1, true );
            send_buffer.Allocate( export_string.size() + 1 );
            std::strcpy( send_buffer.GetAddress< char* >(), export_string.c_str() );

            packet_manager.Send();
            data_object->Clear();
        }

        Profiler::Sampler::GetInstance().Clear();

    } while ( m_Status != kStatus_Exit );
}




/*================================================================================================*/
/*  EOF                                                                                           */
/*================================================================================================*/
