/* -*- mode: c++ ; coding: utf-8-unix -*- */
/*  last updated : 2015/03/25.01:38:00 */

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

#ifndef __COMMON_HPP__
#define __COMMON_HPP__




/*================================================================================================*/
/*  Comment                                                                                       */
/*================================================================================================*/


/*================================================================================================*/
/*  Include Files                                                                                 */
/*================================================================================================*/

#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cassert>

#include <algorithm>
#include <tuple>
#include <string>
#include <vector>

#include "clang-c/Index.h"



/*================================================================================================*/
/*  Class                                                                                         */
/*================================================================================================*/




class   StreamReader
{
public:
    StreamReader( void );
    virtual ~StreamReader( void );
    
    template< typename T >
    void    ReadToken( const char* Format, T& Value, bool bStepNextLine = true )
    {
        ClearLine();
        ::fscanf( m_File, Format, &Value );
        if ( bStepNextLine )
        {
            StepNextLine();
        }
    }

    const char* ReadToken( const char* Format, bool bStepNextLine = true );

    void    Read( char* Buffer, size_t ReadSize );
    
private:
    void    ClearLine( void );
    void    StepNextLine( void );

private:
    enum
    {
        kLineMax = 2048,
    };
    
    FILE*               m_File;
    char                m_Line[ kLineMax ];
};


class   StreamWriter
{
public:
    StreamWriter( void );
    virtual ~StreamWriter( void );

    void    Write( const char* Format, ... );
    void    Flush( void );
    
private:
    FILE*               m_File;
};



class   CFlagsBuffer
{
public:
    CFlagsBuffer( void );
    virtual ~CFlagsBuffer( void );
        
    void    Allocate( const std::vector< std::string >& CFlags );
    void    Deallocate( void );

    int32_t GetNumberOfCFlags( void ) const
    {
        return ( m_NumberOfCFlags );
    }
    char**  GetCFlags( void ) const
    {
        return ( m_CFlags );
    }

private:
    int32_t             m_NumberOfCFlags;
    char**              m_CFlags;
};


class   CSourceCodeBuffer
{
public:
    CSourceCodeBuffer( void );
    virtual ~CSourceCodeBuffer( void );
    
    void    Allocate( int32_t Size );
    void    Deallocate( void );

    int32_t GetSize( void ) const
    {
        return ( m_Size );
    }
    char*   GetBuffer( void ) const
    {
        return ( m_Buffer );
    }

private:
    enum
    {
        kInitialSrcBufferSize = 4096, 
    };

    int32_t             m_Size;
    int32_t             m_BufferCapacity;
    char*               m_Buffer;
};



class   ClangContext
{
public:
    ClangContext( bool excludeDeclarationsFromPCH = false );
    virtual ~ClangContext( void );

    void    Allocate( void );
    void    Deallocate( void );

    const CXIndex   GetCXIndex( void ) const
    {
        return ( m_CxIndex );
    }
    CXIndex GetCXIndex( void )
    {
        return ( m_CxIndex );
    }

    void    SetTranslationUnitFlags( uint32_t Flags )
    {
        m_TranslationUnitFlags = Flags;
    }
    uint32_t    GetTranslationUnitFlags( void ) const
    {
        return ( m_TranslationUnitFlags );
    }

    void    SetCompleteAtFlags( uint32_t Flags )
    {
        m_CompleteAtFlags = Flags;
    }
    uint32_t    GetCompleteAtFlags( void ) const
    {
        return ( m_CompleteAtFlags );
    }
    
    void    SetCompleteResultsLimit( uint32_t NumberOfLimit )
    {
        m_CompleteResultsLimit = NumberOfLimit;
    }
    uint32_t    GetCompleteResultsLimit( void ) const
    {
        return ( m_CompleteResultsLimit );
    }
    
    
private:
    CXIndex             m_CxIndex;

    bool                m_ExcludeDeclarationsFromPCH;
    uint32_t            m_TranslationUnitFlags;
    uint32_t            m_CompleteAtFlags;
    uint32_t            m_CompleteResultsLimit;
};




template< uint32_t Value >
struct BitField
{
    enum
    {
        kValue = Value, 
        kIndex = BitField< (Value >> 1) >::kIndex + 1,
    };
};

template<>
struct BitField< 0 >
{
    enum
    {
        kValue = 0, 
        kIndex = -1, 
    };
};



class   FlagConverter
{
public:
    typedef std::tuple< const char*, uint32_t > Details;
    

    enum
    {
        kMaxValues = 32,
    };


    FlagConverter( void )   :
        m_MaxValue( 0 )
    {
    }
    virtual ~FlagConverter( void )
    {
    }

    void    Clear( void )
    {
        m_MaxValue = 0;
    }


    void    Add( const Details& Values )
    {
        Add( std::get< 0 >( Values ), std::get< 1 >( Values ) );
    }
    
    void    Add( const char* Name, uint32_t BitIndex )
    {
        assert( Name );
        assert( BitIndex < kMaxValues );

        m_FlagNames[ BitIndex ] = Name;
        m_MaxValue              = std::max( m_MaxValue, (BitIndex + 1) );
    }

    uint32_t    GetValue( const std::string& Names ) const
    {
        return ( GetValue( Names.c_str() ) );
    }

    uint32_t    GetValue( const char* Names ) const
    {
        if ( !Names )
        {
            return ( 0 );
        }

        std::string     names( Names );
        const char*     delimit = "|";

        if ( *(names.rbegin()) != *delimit )
        {
            names += delimit;
        }

        uint32_t        value   = 0;
        size_t          begin   = 0;
        size_t          end     = names.find_first_of( delimit );
        
        while ( end != std::string::npos )
        {
            const size_t        length = end - begin;
            const std::string   name   = names.substr( begin, length );

            for ( size_t i = 0; i < m_MaxValue; ++i )
            {
                if ( m_FlagNames[ i ] == name )
                {
                    value |= (1 << i);
                    break;
                }
            }

            begin = end + 1;
            end   = names.find_first_of( delimit, begin );
        }

        return ( value );
    }

private:    
    std::string     m_FlagNames[ kMaxValues ];
    uint32_t        m_MaxValue;
};


#define FLAG_DETAILS( _FLAG_NAME )          FlagConverter::Details( #_FLAG_NAME, BitField< _FLAG_NAME >::kIndex )




class   ClangFlagConverters
{
public:
    ClangFlagConverters( void );


    static  const FlagConverter&    GetCXTranslationUnitFlags( void )
    {
        return ( sm_CXTranslationUnitFlags );
    }
    static  const FlagConverter&    GetCXCodeCompleteFlags( void )
    {
        return ( sm_CXCodeCompleteFlags );
    }

private:
    static  FlagConverter       sm_CXTranslationUnitFlags;
    static  FlagConverter       sm_CXCodeCompleteFlags;
};





#endif  // __COMMON_HPP__
/*================================================================================================*/
/*  EOF                                                                                           */
/*================================================================================================*/
