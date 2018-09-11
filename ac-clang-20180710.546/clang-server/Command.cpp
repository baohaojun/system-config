/* -*- mode: c++ ; coding: utf-8-unix -*- */
/*  last updated : 2018/05/14.19:33:12 */

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

#include "Command.hpp"
#include "Profiler.hpp"




/*================================================================================================*/
/*  Global Class Method Definitions Section                                                       */
/*================================================================================================*/

namespace 
{


static std::shared_ptr< IDataObject > sAllocateDataObject( IDataObject::EType inType )
{
    switch ( inType ) 
    {
        case IDataObject::EType::kLispText:
            {
                std::shared_ptr< IDataObject >  data_object = std::make_shared< DataObject< Lisp::Text::Object > >();

                return data_object;
            }
            break;
        case IDataObject::EType::kLispNode:
            {
                std::shared_ptr< IDataObject >  data_object = std::make_shared< DataObject< Lisp::Node::Object > >();

                return data_object;
            }
            break;
        case IDataObject::EType::kJson:
            {
                std::shared_ptr< IDataObject >  data_object = std::make_shared< DataObject< Json > >();

                return data_object;
            }
            break;
        default:
            assert( 0 );
            break;
    }

    return nullptr;
};


}  // namespace 




void CommandContext::AllocateDataObject( IDataObject::EType inInputType, IDataObject::EType inOutputType )
{
    m_Input  = ::sAllocateDataObject( inInputType );
    m_Output = ::sAllocateDataObject( inOutputType );
}


void CommandContext::SetInputData( const uint8_t* inData )
{
    m_Input->Clear();
    m_Input->SetData( inData );
    m_Input->Decode( *this );
}

std::string CommandContext::GetOutputData( void ) const
{
    return m_Output->ToString();
}


void CommandContext::Clear( void )
{
    m_RequestId = 0;
    m_CommandType.clear();
    m_CommandName.clear();
    m_SessionName.clear();
    m_IsProfile = false;
}


void CommandContext::Read( const Lisp::Text::Object& inData )
{
    Clear();

    // RequestId, command-type, command-name, session-name, is-profile
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
            if ( inSymbol == ":RequestId" )
            {
                m_RequestId = inSExpression.GetValue< uint32_t >();
                ++read_count;
            }
            else if ( inSymbol == ":CommandType" )
            {
                m_CommandType = inSExpression.GetValue< std::string >();
                ++read_count;
            }
            else if ( inSymbol == ":CommandName" )
            {
                m_CommandName = inSExpression.GetValue< std::string >();
                ++read_count;
            }
            else if ( inSymbol == ":SessionName" )
            {
                m_SessionName = inSExpression.GetValue< std::string >();
                ++read_count;
            }
            else if ( inSymbol == ":IsProfile" )
            {
                m_IsProfile = inSExpression.GetValue< bool >();
                ++read_count;
            }

            if ( read_count == 5 )
            {
                return false;
            }

            return true;
        };

    parser.Parse( inData, handler );
}

void CommandContext::Read( const Lisp::Node::Object& inData )
{
    Clear();

    // RequestId, command-type, command-name, session-name, is-profile
    Lisp::Node::PropertyListIterator    iterator = inData.GetRootPropertyListIterator();

    for ( ; !iterator.IsEnd(); iterator.Next() )
    {
        if ( iterator.IsSameKey( ":RequestId" ) )
        {
            m_RequestId = iterator.GetValue< int32_t >();
        }
        else if ( iterator.IsSameKey( ":CommandType" ) )
        {
            m_CommandType = iterator.GetValue< std::string >();
        }
        else if ( iterator.IsSameKey( ":CommandName" ) )
        {
            m_CommandName = iterator.GetValue< std::string >();
        }
        else if ( iterator.IsSameKey( ":SessionName" ) )
        {
            m_SessionName = iterator.GetValue< std::string >();
        }
        else if ( iterator.IsSameKey( ":IsProfile" ) )
        {
            m_IsProfile = iterator.GetValue< bool >();
        }
    }
}

void CommandContext::Read( const Json& inData )
{
    Clear();

    // RequestId, command-type, command-name, session-name, is-profile
    m_RequestId   = inData[ "RequestId" ];
    m_CommandType = inData[ "CommandType" ];
    m_CommandName = inData[ "CommandName" ];
    if ( inData.find( "SessionName" ) != inData.end() )
    {
        m_SessionName = inData[ "SessionName" ];
    }
    if ( inData.find( "IsProfile" ) != inData.end() )
    {
        m_IsProfile = inData[ "IsProfile" ];
    }
}


void CommandContext::Write( Lisp::Text::Object& outData ) const
{
    Lisp::Text::AppendList  plist( outData );

    plist.AddSymbol( ":Profiles" );
    {
        Lisp::Text::NewVector   results_vector( plist );

        const auto&     sampled_profiles = Profiler::Sampler::GetInstance().GetProfiles();

        for ( const auto& profile : sampled_profiles )
        {
            if ( profile.m_IsFinish )
            {
                Lisp::Text::NewList profile_plist( results_vector );

                profile_plist.AddProperty( ":Name", profile.GetName() );
                profile_plist.AddProperty( ":ElapsedTime", profile.GetElapsedTime() );
            }
        }
    }
}

void CommandContext::Write( Json& outData ) const
{
    const auto&     sampled_profiles = Profiler::Sampler::GetInstance().GetProfiles();

    for ( const auto& profile : sampled_profiles )
    {
        if ( profile.m_IsFinish )
        {
            outData[ "Profiles" ].push_back(
                                            {
                                                { "Name", profile.GetName() }, 
                                                { "ElapsedTime", profile.GetElapsedTime() }, 
                                            }
                                            );
        }
    }
}





/*================================================================================================*/
/*  EOF                                                                                           */
/*================================================================================================*/
