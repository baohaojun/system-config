/* -*- mode: c++ ; coding: utf-8-unix -*- */
/*  last updated : 2018/03/15.17:23:06 */

/*
The MIT License

Copyright (c) 2013-2018 yaruopooner [https://github.com/yaruopooner]

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
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
    static  Target  cast( const Source& inValue )
    {
        Target              result;
        std::stringstream   interpreter;

        if ( !( interpreter << inValue ) || !( interpreter >> result ) || !( interpreter >> std::ws ).eof() )
        {
            throw std::invalid_argument( "value cast failure." );
        }

        return result;
    }
};

template< typename Source >
struct lexical_cast_imp< Source, Source >
{
    static  Source  cast( const Source& inValue )
    {
        return inValue;
    }
};

template< typename Target, typename Source >
static  Target  lexical_cast( const Source& inValue )
{
    return lexical_cast_imp< Target, Source >::cast( inValue );
}
    

template< typename T >
struct DefaultReader
{
    virtual ~DefaultReader( void ) = default;
    virtual T    operator ()( const std::string& inArgument ) const
    {
        return lexical_cast< T >( inArgument );
    }
};

template< typename T >
class RangeReader : public DefaultReader< T >
{
public:
    RangeReader( T inMin, T inMax ) : 
        m_Min( inMin )
        , m_Max( inMax )
    {
    }

    virtual T    operator ()( const std::string& inArgument ) const override
    {
        const T   value = lexical_cast< T >( inArgument );

        if ( ( value < m_Min ) || ( m_Max < value ) )
        {
            throw std::domain_error( "value is out of range : " + inArgument );
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
    virtual ~IOptionDetail( void ) = default;

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
    virtual bool HasFlag( uint32_t inFlag ) const = 0;
    virtual const std::string& GetValueDescription( void ) const = 0;

    virtual bool IsSameName( const std::string& inName ) const = 0;
       
    virtual std::shared_ptr< IOptionWithValue >   CreateEvaluator( const std::string& inArgument ) const = 0;
};
    


template< typename T, typename Reader = DefaultReader< T > >
class OptionDetail : public IOptionDetail
{
public:
    OptionDetail( int32_t inId, const std::string& inName, const std::string& inShortName, const std::string& inDescription, uint32_t inFlags = 0, const std::string& inValueDescription = std::string(), const Reader& inReader = Reader() ) :
        m_Id( inId )
        , m_Name( "--" + inName )
        , m_ShortName( "-" + inShortName )
        , m_Description( inDescription )
        , m_Flags( inFlags )
        , m_ValueDescription( inValueDescription )
        , m_Reader( inReader )
    {
    }
    virtual ~OptionDetail( void ) override = default;

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
    bool HasFlag( uint32_t inFlag ) const final
    {
        return ( m_Flags & inFlag ) ? true : false;
    }
    const std::string& GetValueDescription( void ) const final
    {
        return m_ValueDescription;
    }

    bool IsSameName( const std::string& inName ) const final
    {
        return ( ( m_Name == inName ) || ( m_ShortName == inName ) );
    }
       
    std::shared_ptr< IOptionWithValue >   CreateEvaluator( const std::string& inArgument ) const override
    {
        return std::make_shared< OptionWithValueWithReader< T, Reader > >( this, inArgument );
    }
        
    T   GetValue( const std::string& inArgument ) const
    {
        return m_Reader( inArgument );
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
    virtual ~IOptionWithValue( void ) = default;

public:
    virtual const IOptionDetail* GetDetail( void ) const = 0;
    virtual uint32_t GetId( void ) const = 0;
    virtual const std::string& GetOptionName( void ) const = 0;
    virtual bool IsValid( void ) const = 0;
    virtual bool Evaluate( std::string& inMessage ) = 0;
};
    

template< typename T >
class OptionWithValue : public IOptionWithValue
{
protected:
    OptionWithValue( const IOptionDetail* inDetail, const std::string& inArgument ) :
        m_Detail( inDetail )
        , m_Argument( inArgument )
        , m_ValidValue( false )
    {
    }

public:
    virtual ~OptionWithValue( void ) override = default;

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

    virtual bool Evaluate( std::string& inMessage ) override
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
    OptionWithValueWithReader( const IOptionDetail* inDetail, const std::string& inArgument ) : 
        OptionWithValue< T >( inDetail, inArgument )
    {
    }
    virtual ~OptionWithValueWithReader( void ) override = default;

    bool Evaluate( std::string& outMessage ) override
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
        catch ( const std::exception& inException )
        {
            std::stringstream   ss;
                
            ss << this->m_Detail->GetName() << " : " << inException.what() << std::endl;
            ss << this->m_Detail->GetDescription();
            outMessage = ss.str();
                
            return false;
        }
    }
};



using   OptionDetailArray    = std::vector< std::shared_ptr< IOptionDetail > >;
using   OptionWithValueArray = std::vector< std::shared_ptr< IOptionWithValue > >;

class Parser
{
public:

    void    AddOption( int32_t inId, const std::string& inName, const std::string& inShortName, const std::string& inDescription, uint32_t inFlags = 0, const std::string& inValueDescription = std::string() )
    {
        m_Details.emplace_back( std::make_shared< OptionDetail< std::string > >( inId, inName, inShortName, inDescription, inFlags, inValueDescription ) );
    }

    template< typename T, typename Reader = DefaultReader< T > >
    void    AddOption( int32_t inId, const std::string& inName, const std::string& inShortName, const std::string& inDescription, uint32_t inFlags = 0, const std::string& inValueDescription = std::string(), const Reader& inReader = Reader() )
    {
        m_Details.emplace_back( std::make_shared< OptionDetail< T, Reader > >( inId, inName, inShortName, inDescription, inFlags, inValueDescription, inReader ) );
    }
    
    size_t GetNumberOfOptionValues( void ) const
    {
        return m_OptionValues.size();
    }
    
    size_t GetNumberOfArguments( void ) const
    {
        return m_Arguments.size();
    }

    static const std::string& GetOptionName( const IOptionWithValue* inValue )
    {
        return inValue->GetDetail()->GetName();
    }
    
    template< typename T >
    static const T& GetValue( const std::shared_ptr< IOptionWithValue >& inValue )
    {
        const auto* casted_value = dynamic_cast< const OptionWithValue< T >* >( inValue.get() );
        
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

    bool    Parse( int inArgc, char* inArgv[] )
    {
        // clear & store
        m_Arguments.clear();
        for ( int i = 1; i < inArgc; ++i )
        {
            m_Arguments.emplace_back( inArgv[ i ] );
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
                m_Errors.emplace_back( "option syntax error : argument is not option format : " + option_name );
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
                            m_Errors.emplace_back( "option syntax error : not enough argument : " + option_name );
                            break;
                        }

                        const std::string&    next_value = m_Arguments[ next_i ];

                        if ( HasOptionPrefix( next_value ) )
                        {
                            // option have not value
                            m_Errors.emplace_back( "option syntax error : value not found : " + option_name );
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
                m_OptionValues.emplace_back( detail->CreateEvaluator( value ) );
                
                is_valid_format = true;
                break;
            }

            if ( !is_match )
            {
                // unknown option
                m_Warnings.emplace_back( "unknown option : " + option_name );

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
                m_Errors.emplace_back( "option syntax error : illegal option format : " + option_name );
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
                m_Errors.emplace_back( "option syntax error : duplicate use : " + detal->GetName() );
                continue;
            }

            std::string message;

            if ( !option_value->Evaluate( message ) )
            {
                m_Errors.emplace_back( message );
                // bad value
            }
        }
    
        return ( m_Errors.size() == 0 );
    }

    void    PrintUsage( const std::string& inFormat ) const
    {
        std::cout << "Usage: " << inFormat <<std::endl;
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
    static bool HasOptionPrefix( const std::string& inName )
    {
        return ( ( inName.compare( 0, 2, "--" ) == 0 ) || ( inName.compare( 0, 1, "-" ) == 0 ) );
    }

    
private:    
    OptionDetailArray               m_Details;
    std::vector< std::string >      m_Arguments;
    std::vector< std::string >      m_Errors;
    std::vector< std::string >      m_Warnings;
    OptionWithValueArray            m_OptionValues;
};
    



}  // namespace CommandLine



#endif
/*================================================================================================*/
/*  EOF                                                                                           */
/*================================================================================================*/
