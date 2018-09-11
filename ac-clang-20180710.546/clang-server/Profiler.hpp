/* -*- mode: c++ ; coding: utf-8-unix -*- */
/*  last updated : 2018/03/24.20:30:03 */

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

#ifndef __PROFILER_HPP__
#define __PROFILER_HPP__



/*================================================================================================*/
/*  Include Files                                                                                 */
/*================================================================================================*/

#include <string>
#include <vector>
#include <tuple>
#include <chrono>
#include <cassert>



/*================================================================================================*/
/*  Class                                                                                         */
/*================================================================================================*/

namespace Profiler
{



struct Profile
{
    using   time_point = std::chrono::high_resolution_clock::time_point;

    Profile( const char* inName ) :
        m_Name( inName )
    {
    }
    // virtual ~Profile( void ) = default;

    const std::string& GetName( void ) const
    {
        return m_Name;
    }
    float GetElapsedTime( void ) const
    {
        return m_ElapsedTime;
    }

    time_point GetNow( void ) const
    {
        return std::chrono::high_resolution_clock::now();
    }

    void SetBeginTime( void )
    {
        m_BeginTime = GetNow();
    }
    void SetEndTime( void )
    {
        m_EndTime = GetNow();
    }
    void SetElapsedTime( void )
    {
        m_ElapsedTime = std::chrono::duration_cast< std::chrono::microseconds >( m_EndTime - m_BeginTime ).count() / 1000.0f;
    }

    float CalcElapsedTime( void ) const
    {
        return std::chrono::duration_cast< std::chrono::microseconds >( m_EndTime - m_BeginTime ).count() / 1000.0f;
    }

    std::string m_Name;
    time_point  m_BeginTime;
    time_point  m_EndTime;
    // time_point  m_DeltaTime;
    float       m_ElapsedTime = 0.0f;
    bool        m_IsFinish    = false;
};



template< class T >
class Singleton
{
protected:
    Singleton( void ) = default;
    virtual ~Singleton( void ) = default;

    Singleton( const Singleton& ) = delete;
    void operator =( const Singleton& ) = delete;

public:
    static T& GetInstance( void )
    {
        static T    instance;
        return instance;
    }
};



class Sampler final : public Singleton< Sampler >
{
public:
    enum Specification
    {
        kInitialSize = 128, 
    };

    Sampler( void )
    {
        m_Profiles.reserve( kInitialSize );
    }
    virtual ~Sampler( void ) = default;

    void Clear( void )
    {
        m_Profiles.clear();
    }

    size_t Push( const char* inName )
    {
        const size_t    index = m_Profiles.size();

        m_Profiles.emplace_back( Profile( inName ) );

        Profile&  profile = m_Profiles[ index ];

        profile.SetBeginTime();

        return index;
    }
    void Pop( const size_t inIndex )
    {
        assert( inIndex < m_Profiles.size() );

        Profile&  profile = m_Profiles[ inIndex ];

        profile.SetEndTime();
        profile.SetElapsedTime();
        profile.m_IsFinish = true;
    }


    const std::vector< Profile >& GetProfiles( void ) const
    {
        return m_Profiles;
    }

private:
    std::vector< Profile >      m_Profiles;
};



class ScopedSample
{
public:
    ScopedSample( const char* inName ) : 
        m_Index( Sampler::GetInstance().Push( inName ) )
    {
    }

    virtual ~ScopedSample( void )
    {
        Sampler::GetInstance().Pop( m_Index );
    }

private:
    size_t          m_Index = -1;
};



#define PROFILER_SCOPED_SAMPLE( _NAME )                     Profiler::ScopedSample      sample_profile( _NAME )
#define PROFILER_SCOPED_SAMPLE_FUNCTION()                   Profiler::ScopedSample      sample_profile( __FUNCTION__ )
#define PROFILER_SAMPLE_BEGIN( _TMP_VAR_NAME, _NAME )       const auto _TMP_VAR_NAME##_sample_index = Profiler::Sampler::GetInstance().Push( _NAME )
#define PROFILER_SAMPLE_END( _TMP_VAR_NAME )                Profiler::Sampler::GetInstance().Pop( _TMP_VAR_NAME##_sample_index )




}  // namespace Profiler





#endif
/*================================================================================================*/
/*  EOF                                                                                           */
/*================================================================================================*/
