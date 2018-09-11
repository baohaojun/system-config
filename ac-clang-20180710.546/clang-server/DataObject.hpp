/* -*- mode: c++ ; coding: utf-8-unix -*- */
/*  last updated : 2018/05/14.19:33:17 */

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

#ifndef __DATA_OBJECT_HPP__
#define __DATA_OBJECT_HPP__




/*================================================================================================*/
/*  Include Files                                                                                 */
/*================================================================================================*/

#include <string>

#include "parser/Lisp.hpp"
#include "parser/json.hpp"

using Json = nlohmann::json;




/*================================================================================================*/
/*  Class                                                                                         */
/*================================================================================================*/


template< typename DataType >
class ISerializable
{
protected:
    ISerializable( void ) = default;
    virtual ~ISerializable( void ) = default;

public:
    virtual void Read( const DataType& /*inData*/ )
    {
    }
    virtual void Write( DataType& /*outData*/ ) const
    {
    }
};


class IMultiSerializable: public ISerializable< Lisp::Text::Object >, public ISerializable< Lisp::Node::Object >, public ISerializable< Json >
{
protected:
    IMultiSerializable( void ) = default;
    virtual ~IMultiSerializable( void ) = default;
};



class IDataObject
{
public:
    enum EType
    {
        kInvalid = -1, 
        kLispText = 0, 
        kLispNode, 
        kJson, 
    };

protected:
    IDataObject( EType inType = EType::kInvalid ) : 
        m_Type( inType )
    {
    }
    virtual ~IDataObject( void ) = default;
    
public:

    EType GetType( void ) const
    {
        return m_Type;
    }

    template< typename DataType >
    bool IsSame( void ) const
    {
        return ( TypeTraits< DataType >::Value == m_Type );
    }
    

    template< typename SerializableVisitor >
    void Encode( const SerializableVisitor& inVisitor );

    template< typename SerializableVisitor >
    void Decode( SerializableVisitor& outVisitor ) const;

    virtual void SetData( const uint8_t* inAddress ) = 0;
    virtual std::string ToString( void ) const = 0;
    virtual void Clear( void ) = 0;
    
    
    template< typename DataType > struct TypeTraits { enum { Value = EType::kInvalid, }; };
#if 0
    // Template specialization in the class is prohibited.
    template<> struct TypeTraits< Lisp::Text::Object > { enum { Value = EType::kLispText, }; };
    template<> struct TypeTraits< Lisp::Node::Object > { enum { Value = EType::kLispNode, }; };
    template<> struct TypeTraits< Json > { enum { Value = EType::kJson, }; };
#endif

    // template< EType Value > struct Traits {  };
    // template<> struct Traits< EType::kLispText > { using Type = Lisp::Text::Object; };
    // template<> struct Traits< EType::kLispNode > { using Type = Lisp::Node::Object; };
    // template<> struct Traits< EType::kJson > { using Type = Json; };

protected:
    EType           m_Type;
};

template<> struct IDataObject::TypeTraits< Lisp::Text::Object > { enum { Value = EType::kLispText, }; };
template<> struct IDataObject::TypeTraits< Lisp::Node::Object > { enum { Value = EType::kLispNode, }; };
template<> struct IDataObject::TypeTraits< Json > { enum { Value = EType::kJson, }; };



template< typename DataType >
class DataObject : public IDataObject
{
public:
    DataObject( void ) : 
        IDataObject( static_cast< EType >( IDataObject::TypeTraits< DataType >::Value ) )
    {
    }
    virtual ~DataObject( void ) override = default;
    

    // Type&    GetData( void )
    // {
    //     return m_Data;
    // }
    
    // const Type&    GetData( void ) const
    // {
    //     return m_Data;
    // }

    void Encode( const ISerializable< DataType >& inVisitor )
    {
        inVisitor.Write( m_Data );
    }

    void Decode( ISerializable< DataType >& outVisitor ) const
    {
        outVisitor.Read( m_Data );
    }

    virtual void SetData( const uint8_t* ) override
    {
    }
    virtual std::string ToString( void ) const override
    {
        return std::string();
    }
    virtual void Clear( void ) override
    {
    }


protected:
    DataType        m_Data;
};


template<> inline
void DataObject< Lisp::Text::Object >::SetData( const uint8_t* inAddress )
{
    m_Data.Set( reinterpret_cast< const char* >( inAddress ) );
}

template<> inline
void DataObject< Lisp::Node::Object >::SetData( const uint8_t* inAddress )
{
    Lisp::Node::Parser      parser;

    parser.Parse( reinterpret_cast< const char* >( inAddress ), m_Data );
}

template<> inline
void DataObject< Json >::SetData( const uint8_t* inAddress )
{
    m_Data = Json::parse( inAddress );
}


template<> inline
std::string DataObject< Lisp::Text::Object >::ToString( void ) const
{
    return m_Data.GetString();
}

template<> inline
std::string DataObject< Json >::ToString( void ) const
{
    if ( m_Data.empty() )
    {
        return std::string();
    }

    return m_Data.dump();
}


template<> inline
void DataObject< Lisp::Text::Object >::Clear( void )
{
    m_Data.Clear();
}

template<> inline
void DataObject< Lisp::Node::Object >::Clear( void )
{
    m_Data.Clear();
}

template<> inline
void DataObject< Json >::Clear( void )
{
    m_Data.clear();
}





/*================================================================================================*/
/*  Class Inline Method                                                                           */
/*================================================================================================*/


template< typename SerializableVisitor > inline
void IDataObject::Encode( const SerializableVisitor& inVisitor )
{
    switch ( m_Type )
    {
        case EType::kLispText:
            {
                auto    data_object = static_cast< DataObject< Lisp::Text::Object >* >( this );

                data_object->Encode( inVisitor );
            }
            break;
        case EType::kLispNode:
            {
                auto    data_object = static_cast< DataObject< Lisp::Node::Object >* >( this );

                data_object->Encode( inVisitor );
            }
            break;
        case EType::kJson:
            {
                auto    data_object = static_cast< DataObject< Json >* >( this );

                data_object->Encode( inVisitor );
            }
            break;
        default:
            break;
    }
}


template< typename SerializableVisitor > inline
void IDataObject::Decode( SerializableVisitor& outVisitor ) const
{
    switch ( m_Type )
    {
        case EType::kLispText:
            {
                auto    data_object = static_cast< const DataObject< Lisp::Text::Object >* >( this );

                data_object->Decode( outVisitor );
            }
            break;
        case EType::kLispNode:
            {
                auto    data_object = static_cast< const DataObject< Lisp::Node::Object >* >( this );

                data_object->Decode( outVisitor );
            }
            break;
        case EType::kJson:
            {
                auto    data_object = static_cast< const DataObject< Json >* >( this );

                data_object->Decode( outVisitor );
            }
            break;
        default:
            break;
    }
}





#endif
/*================================================================================================*/
/*  EOF                                                                                           */
/*================================================================================================*/
