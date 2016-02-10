/* -*- mode: c++ ; coding: utf-8-unix -*- */
/*  last updated : 2015/05/23.23:17:32 */

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

#ifndef __COMMAND_LINE_HPP__
#define __COMMAND_LINE_HPP__


/*================================================================================================*/
/*  Include Files                                                                                 */
/*================================================================================================*/

#include <string>
#include <vector>
#include <set>
#include <memory>
#include <iostream>
#include <sstream>


/*================================================================================================*/
/*  Class                                                                                         */
/*================================================================================================*/


namespace CommandLine
{


template< typename Target, typename Source >
struct lexical_cast_imp
{
    static  Target  cast( const Source& value )
    {
        Target              result;
        std::stringstream   interpreter;

        if ( !( interpreter << value ) || !( interpreter >> result ) || !( interpreter >> std::ws ).eof() )
        {
            throw std::invalid_argument( "value cast failure." );
        }

        return result;
    }
};

template< typename Source >
struct lexical_cast_imp< Source, Source >
{
    static  Source  cast( const Source& value )
    {
        return value;
    }
};

template< typename Target, typename Source >
static  Target  lexical_cast( const Source& value )
{
    return lexical_cast_imp< Target, Source >::cast( value );
}
    

template< typename T >
struct DefaultReader
{
    virtual ~DefaultReader( void )
    {
    }
    virtual T    operator ()( const std::string& argument ) const
    {
        return lexical_cast< T >( argument );
    }
};

template< typename T >
class RangeReader : public DefaultReader< T >
{
public:
    RangeReader( T min, T max ) : 
        m_Min( min )
        , m_Max( max )
    {
    }

    virtual T    operator ()( const std::string& argument ) const override
    {
        const T   value = lexical_cast< T >( argument );

        if ( ( value < m_Min ) || ( m_Max < value ) )
        {
            throw std::domain_error( "value is out of range : " + argument );
        }

        return value;
    }

    T   m_Min;
    T   m_Max;
};


class IOptionWithValue;
template< typename T, typename Reader >
class OptionWithValueWithReader;


class IOptionDetail
{
protected:
    virtual ~IOptionDetail()
    {
    }

public:
    enum Flag
    {
        kFlag_Once         = 1 << 0, 
        kFlag_HasValue     = 1 << 1, 
        kFlag_RequireValue = 1 << 2, 
    };
    
    virtual int32_t GetId( void ) const = 0;
    virtual const std::string& GetName( void ) const = 0;
    virtual const std::string& GetShortName( void ) const = 0;
    virtual const std::string& GetDescription( void ) const = 0;
    virtual bool HasFlag( uint32_t flag ) const = 0;
    virtual const std::string& GetValueDescription( void ) const = 0;

    virtual bool IsSameName( const std::string& name ) const = 0;
       
    virtual std::shared_ptr< IOptionWithValue >   CreateEvaluator( const std::string& argument ) const = 0;
};
    


template< typename T, typename Reader = DefaultReader< T > >
class OptionDetail : public IOptionDetail
{
public:
    OptionDetail( int32_t id, const std::string& name, const std::string& shortName, const std::string& description, uint32_t flags = 0, const std::string& valueDescription = std::string(), const Reader& reader = Reader() ) :
        m_Id( id )
        , m_Name( "--" + name )
        , m_ShortName( "-" + shortName )
        , m_Description( description )
        , m_Flags( flags )
        , m_ValueDescription( valueDescription )
        , m_Reader( reader )
    {
    }
    virtual ~OptionDetail()
    {
    }

    int32_t GetId( void ) const final
    {
        return m_Id;
    }
    const std::string& GetName( void ) const final
    {
        return m_Name;
    }
    const std::string& GetShortName( void ) const final
    {
        return m_ShortName;
    }
    const std::string& GetDescription( void ) const final
    {
        return m_Description;
    }
    bool HasFlag( uint32_t flag ) const final
    {
        return ( m_Flags & flag ) ? true : false;
    }
    const std::string& GetValueDescription( void ) const final
    {
        return m_ValueDescription;
    }

    bool IsSameName( const std::string& name ) const final
    {
        return ( ( m_Name == name ) || ( m_ShortName == name ) );
    }
       
    std::shared_ptr< IOptionWithValue >   CreateEvaluator( const std::string& argument ) const override
    {
        return std::make_shared< OptionWithValueWithReader< T, Reader > >( this, argument );
    }
        
    T   GetValue( const std::string& argument ) const
    {
        return m_Reader( argument );
    }
        
        
protected:
    const int32_t       m_Id;
    const std::string   m_Name;
    const std::string   m_ShortName;
    const std::string   m_Description;
    const uint32_t      m_Flags;
    const std::string   m_ValueDescription;
    const Reader        m_Reader;
};

    

class IOptionWithValue
{
protected:
    virtual ~IOptionWithValue( void )
    {
    }

public:
    virtual const IOptionDetail* GetDetail( void ) const = 0;
    virtual uint32_t GetId( void ) const = 0;
    virtual const std::string& GetOptionName( void ) const = 0;
    virtual bool IsValid( void ) const = 0;
    virtual bool Evaluate( std::string& message ) = 0;
};
    

template< typename T >
class OptionWithValue : public IOptionWithValue
{
protected:
    OptionWithValue( const IOptionDetail* detail, const std::string& argument ) :
        m_Detail( detail )
        , m_Argument( argument )
        , m_ValidValue( false )
    {
    }

public:
    virtual ~OptionWithValue( void )
    {
    }

    const IOptionDetail* GetDetail( void ) const final
    {
        return m_Detail;
    }

    uint32_t GetId( void ) const final
    {
        return m_Detail->GetId();
    }
        
    const std::string& GetOptionName( void ) const final
    {
        return m_Detail->GetName();
    }

    bool IsValid( void ) const final
    {
        return m_ValidValue;
    }

    virtual bool Evaluate( std::string& message ) override
    {
        return true;
    }

    const T&   GetValue( void ) const
    {
        return m_Value;
    }

protected:
    const IOptionDetail*    m_Detail;
    const std::string       m_Argument;
    bool                    m_ValidValue;
    T                       m_Value;
};


template< typename T, typename Reader >
class OptionWithValueWithReader : public OptionWithValue< T >
{
public:
    OptionWithValueWithReader( const IOptionDetail* detail, const std::string& argument ) : 
        OptionWithValue< T >( detail, argument )
    {
    }
    virtual ~OptionWithValueWithReader( void )
    {
    }

    bool Evaluate( std::string& message ) override
    {
        try
        {
            if ( !this->m_Argument.empty() )
            {
                const auto*    detail = dynamic_cast< const OptionDetail< T, Reader >* >( this->m_Detail );

                this->m_Value      = detail->GetValue( this->m_Argument );
                this->m_ValidValue = true;
            }

            return true;
        }
        catch ( const std::exception& exception )
        {
            std::stringstream   ss;
                
            ss << this->m_Detail->GetName() << " : " << exception.what() << std::endl;
            ss << this->m_Detail->GetDescription();
            message = ss.str();
                
            return false;
        }
    }
};



typedef std::vector< std::shared_ptr< IOptionDetail > >     OptionDetailArray;
typedef std::vector< std::shared_ptr< IOptionWithValue > >  OptionWithValueArray;

class Parser
{
public:

    void    AddOption( int32_t id, const std::string& name, const std::string& shortName, const std::string& description, uint32_t flags = 0, const std::string& valueDescription = std::string() )
    {
        m_Details.push_back( std::make_shared< OptionDetail< std::string > >( id, name, shortName, description, flags, valueDescription ) );
    }

    template< typename T, typename Reader = DefaultReader< T > >
    void    AddOption( int32_t id, const std::string& name, const std::string& shortName, const std::string& description, uint32_t flags = 0, const std::string& valueDescription = std::string(), const Reader& reader = Reader() )
    {
        m_Details.push_back( std::make_shared< OptionDetail< T, Reader > >( id, name, shortName, description, flags, valueDescription, reader ) );
    }
    
    size_t GetNumberOfOptionValues( void ) const
    {
        return m_OptionValues.size();
    }
    
    size_t GetNumberOfArguments( void ) const
    {
        return m_Arguments.size();
    }

    static const std::string& GetOptionName( const IOptionWithValue* value )
    {
        return value->GetDetail()->GetName();
    }
    
    template< typename T >
    static const T& GetValue( const std::shared_ptr< IOptionWithValue >& value )
    {
        const auto* casted_value = dynamic_cast< const OptionWithValue< T >* >( value.get() );
        
        return casted_value->GetValue();
    }

    const OptionDetailArray&    GetOptionDetailArray( void ) const
    {
        return m_Details;
    }
    const OptionWithValueArray&    GetOptionWithValueArray( void ) const
    {
        return m_OptionValues;
    }

    bool    Parse( int argc, char* argv[] )
    {
        // clear & store
        m_Arguments.clear();
        for ( int i = 1; i < argc; ++i )
        {
            m_Arguments.push_back( argv[ i ] );
        }

        // parse
        const size_t   n_args = m_Arguments.size();
    
        for ( size_t i = 0; i < n_args; ++i )
        {
            const std::string&  option_name = m_Arguments[ i ];

            if ( !HasOptionPrefix( option_name ) )
            {
                // error, argument is not option format
                // ignore
                m_Errors.push_back( "option syntax error : argument is not option format : " + option_name );
                continue;
            }

            bool        is_match        = false;
            bool        is_valid_format = false;
            std::string value;

            for ( const auto detail : m_Details )
            {
                if ( !detail->IsSameName( option_name ) )
                {
                    continue;
                }

                is_match = true;
            
                // found
                if ( detail->HasFlag( IOptionDetail::kFlag_HasValue ) )
                {
                    const size_t   next_i = i + 1;

                    if ( detail->HasFlag( IOptionDetail::kFlag_RequireValue ) )
                    {
                        if ( n_args <= next_i )
                        {
                            // error, argument locator over
                            m_Errors.push_back( "option syntax error : not enough argument : " + option_name );
                            break;
                        }

                        const std::string&    next_value = m_Arguments[ next_i ];

                        if ( HasOptionPrefix( next_value ) )
                        {
                            // option have not value
                            m_Errors.push_back( "option syntax error : value not found : " + option_name );
                            break;
                        }

                        value = m_Arguments[ next_i ];
                        i     = next_i;
                    }
                    else
                    {
                        if ( next_i < n_args )
                        {
                            const std::string&    next_value = m_Arguments[ next_i ];

                            if ( !HasOptionPrefix( next_value ) )
                            {
                                // option have not value
                                value = m_Arguments[ next_i ];
                                i     = next_i;
                            }
                        }
                        else
                        {
                            // not value, option only
                        }
                    }
                }

                // store found detal & value
                m_OptionValues.push_back( detail->CreateEvaluator( value ) );
                
                is_valid_format = true;
                break;
            }

            if ( !is_match )
            {
                // unknown option
                m_Warnings.push_back( "unknown option : " + option_name );

                // value check
                const size_t   next_i = i + 1;

                if ( next_i < n_args )
                {
                    const std::string&    next_value = m_Arguments[ next_i ];

                    if ( !HasOptionPrefix( next_value ) )
                    {
                        // discard unknown option value
                        i = next_i;
                    }
                }
            }
            else if ( !is_valid_format )
            {
                // illegal option format
                m_Errors.push_back( "option syntax error : illegal option format : " + option_name );
            }
        }

        std::set< const IOptionDetail* >    used_options;
    
        for ( auto& option_value : m_OptionValues )
        {
            const IOptionDetail*    detal = option_value->GetDetail();
            const bool              found = ( used_options.find( detal ) != used_options.end() );

            if ( !found )
            {
                // first use
                used_options.insert( detal );
            }

            if ( found && detal->HasFlag( IOptionDetail::kFlag_Once ) )
            {
                // duplicate use
                m_Errors.push_back( "option syntax error : duplicate use : " + detal->GetName() );
                continue;
            }

            std::string message;

            if ( !option_value->Evaluate( message ) )
            {
                m_Errors.push_back( message );
                // bad value
            }
        }
    
        return ( m_Errors.size() == 0 );
    }

    void    PrintUsage( const std::string& format ) const
    {
        std::cout << "Usage: " << format <<std::endl;
        std::cout << std::endl;
        std::cout << "OPTIONS:" << std::endl;
        for ( const auto& detail : m_Details )
        {
            std::cout << "  " << detail->GetName();
            if ( !detail->GetShortName().empty() )
            {
                std::cout << ", " << detail->GetShortName();
            }
            if ( !detail->GetValueDescription().empty() )
            {
                std::cout << " <" << detail->GetValueDescription() << ">";
            }
            std::cout << std::endl;
            std::cout << "                          " << detail->GetDescription() << std::endl;
        }
    }
    
    void    PrintErrors( void ) const
    {
        if ( !m_Errors.empty() )
        {
            std::cout << "Error:" << std::endl;
            for ( const auto& error : m_Errors )
            {
                std::cout << error << std::endl;
            }
        }
    }

    void    PrintWarnings( void ) const
    {
        if ( !m_Warnings.empty() )
        {
            std::cout << "Warning:" << std::endl;
            for ( const auto& warning : m_Warnings )
            {
                std::cout << warning << std::endl;
            }
        }
    }

private:
    static bool HasOptionPrefix( const std::string& name )
    {
        return ( ( name.compare( 0, 2, "--" ) == 0 ) || ( name.compare( 0, 1, "-" ) == 0 ) );
    }

    
private:    
    OptionDetailArray               m_Details;
    std::vector< std::string >      m_Arguments;
    std::vector< std::string >      m_Errors;
    std::vector< std::string >      m_Warnings;
    OptionWithValueArray            m_OptionValues;
};
    



};



#endif
/*================================================================================================*/
/*  EOF                                                                                           */
/*================================================================================================*/
