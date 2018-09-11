/* -*- mode: c++ ; coding: utf-8-unix -*- */
/*  last updated : 2018/05/14.19:35:08 */

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
/*  Include Files                                                                                 */
/*================================================================================================*/


#include "parser/CommandLine.hpp"
#include "ClangServer.hpp"



/*================================================================================================*/
/*  Global Function Definitions Section                                                           */
/*================================================================================================*/


enum
{
    kStreamBuffer_UnitSize = 1 * 1024 * 1024, 
    kStreamBuffer_MinMB    = 1, 
    kStreamBuffer_MaxMB    = 5, 
};

enum 
{
    kOption_Help, 
    kOption_Version, 
    kOption_LogFile, 
    kOption_STDIN_BufferSize, 
    kOption_STDOUT_BufferSize, 
    kOption_InputData, 
    kOption_OutputData, 
};


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



int main( int inArgc, char *inArgv[] )
{
    std::ios_base::sync_with_stdio( false );

    // parse options
    const std::string           server_version     = CLANG_SERVER_VERSION;
    const std::string           clang_version      = ::sGetClangVersion();
    const std::string           generate           = CMAKE_GENERATOR "/" CMAKE_HOST_SYSTEM_PROCESSOR;
    std::string                 logfile;
    ClangServer::EIoDataType    input_data_type    = ClangServer::EIoDataType::kSExpression;
    ClangServer::EIoDataType    output_data_type   = ClangServer::EIoDataType::kSExpression;
    size_t                      stdin_buffer_size  = kStreamBuffer_MinMB;
    size_t                      stdout_buffer_size = kStreamBuffer_MinMB;

    {
        CommandLine::Parser        declare_options;

        declare_options.AddOption( kOption_Help, "help", "h", "Display available options.", 
                                   CommandLine::IOptionDetail::kFlag_Once );
        declare_options.AddOption( kOption_Version, "version", "v", "Display current version.", 
                                   CommandLine::IOptionDetail::kFlag_Once );
        // declare_options.AddOption< std::string >( kOption_LogFile, "logfile", "l", "Enable IPC records output.(for debug)",
        //                                           ( CommandLine::IOptionDetail::kFlag_Once | CommandLine::IOptionDetail::kFlag_HasValue ), "file path" );
        declare_options.AddOption< uint32_t >( kOption_STDIN_BufferSize, "stdin-buffer-size", "sibs", "STDIN buffer size. <size> is 1 - 5 MB", 
                                               ( CommandLine::IOptionDetail::kFlag_Once | CommandLine::IOptionDetail::kFlag_HasValue ), "size", 
                                               CommandLine::RangeReader< uint32_t >( kStreamBuffer_MinMB, kStreamBuffer_MaxMB ) );
        declare_options.AddOption< uint32_t >( kOption_STDOUT_BufferSize, "stdout-buffer-size", "sobs", "STDOUT buffer size. <size> is 1 - 5 MB", 
                                               ( CommandLine::IOptionDetail::kFlag_Once | CommandLine::IOptionDetail::kFlag_HasValue ), "size", 
                                               CommandLine::RangeReader< uint32_t >( kStreamBuffer_MinMB, kStreamBuffer_MaxMB ) );
        declare_options.AddOption< std::string >( kOption_InputData, "input-data", "idata", "input data type. <type> is s-expression | json", 
                                                  ( CommandLine::IOptionDetail::kFlag_Once | CommandLine::IOptionDetail::kFlag_HasValue ), "type" );
        declare_options.AddOption< std::string >( kOption_OutputData, "output-data", "odata", "output data type. <type> is s-expression | json", 
                                                  ( CommandLine::IOptionDetail::kFlag_Once | CommandLine::IOptionDetail::kFlag_HasValue ), "type" );

        if ( declare_options.Parse( inArgc, inArgv ) )
        {
            auto    get_io_data_type = []( const std::string& inDataType ) -> ClangServer::EIoDataType
                {
                    if ( inDataType == "s-expression" )
                    {
                        return ClangServer::EIoDataType::kSExpression;
                    }
                    else if ( inDataType == "json" )
                    {
                        return ClangServer::EIoDataType::kJson;
                    }

                    return ClangServer::EIoDataType::kSExpression;
                };

            for ( const auto& option_value : declare_options.GetOptionWithValueArray() )
            {
                switch ( option_value->GetId() )
                {
                    case    kOption_Help:
                        declare_options.PrintUsage( "clang-server [options] <values>" );
                        return 0;
                    case    kOption_Version:
                        std::cout << server_version << " (" << generate << ")" << std::endl;
                        std::cout << clang_version << std::endl;
                        return 0;
                    case    kOption_LogFile:
                        if ( option_value->IsValid() )
                        {
                            std::string result = declare_options.GetValue< std::string >( option_value );

                            logfile = std::move( result );
                        }
                        break;
                    case    kOption_STDIN_BufferSize:
                        if ( option_value->IsValid() )
                        {
                            const uint32_t  result = declare_options.GetValue< uint32_t >( option_value );

                            stdin_buffer_size = result;
                        }
                        break;
                    case    kOption_STDOUT_BufferSize:
                        if ( option_value->IsValid() )
                        {
                            const uint32_t  result = declare_options.GetValue< uint32_t >( option_value );

                            stdout_buffer_size = result;
                        }
                        break;
                    case    kOption_InputData:
                        if ( option_value->IsValid() )
                        {
                            const std::string   result = declare_options.GetValue< std::string >( option_value );

                            input_data_type = get_io_data_type( result );
                        }
                        break;
                    case    kOption_OutputData:
                        if ( option_value->IsValid() )
                        {
                            const std::string   result = declare_options.GetValue< std::string >( option_value );

                            output_data_type = get_io_data_type( result );
                        }
                        break;
                }
            }
            declare_options.PrintWarnings();
        }
        else
        {
            declare_options.PrintErrors();
            return 1;
        }
    }


    // stream buffer expand
    // std::shared_ptr< char >   stdin_buffer( new char[ kStreamBuffer_UnitSize ], std::default_delete< char[] >() );
    // std::shared_ptr< char >   stdout_buffer( new char[ kStreamBuffer_UnitSize ], std::default_delete< char[] >() );

    stdin_buffer_size  *= kStreamBuffer_UnitSize;
    stdout_buffer_size *= kStreamBuffer_UnitSize;


    // server instance
    ClangServer::Specification  initial_spec( stdin_buffer_size, stdout_buffer_size, input_data_type, output_data_type, logfile );
    ClangServer                 server( initial_spec );

    server.ParseCommand();


    return 0;
}




/*================================================================================================*/
/*  EOF                                                                                           */
/*================================================================================================*/
