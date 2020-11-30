#!/usr/bin/env bash
set -e

test-1-command() {
    test_command=$1
    job_xml=$(
        cat <<'EOF86c1be707507' | . .replace-%% -- | grep -v '^#.*nxml-mode'
# {%nxml-mode%}
<?xml version="1.0" encoding="UTF-8"?>
<project>
  <actions />
  <description><![CDATA[<!--start html-->
<!--end html-->
]]></description>
  <keepDependencies>false</keepDependencies>
  <properties>

    <org.bstick12.jenkinsci.plugins.leastload.LeastLoadDisabledProperty plugin="leastload@2.0.1">
      <leastLoadDisabled>false</leastLoadDisabled>
    </org.bstick12.jenkinsci.plugins.leastload.LeastLoadDisabledProperty>
    <com.sonyericsson.rebuild.RebuildSettings plugin="rebuild@1.28">
      <autoRebuild>false</autoRebuild>
      <rebuildDisabled>false</rebuildDisabled>
    </com.sonyericsson.rebuild.RebuildSettings>
    <hudson.plugins.throttleconcurrents.ThrottleJobProperty plugin="throttle-concurrents@2.0.1">
      <maxConcurrentPerNode>0</maxConcurrentPerNode>
      <maxConcurrentTotal>0</maxConcurrentTotal>
      <categories class="java.util.concurrent.CopyOnWriteArrayList" />
      <throttleEnabled>false</throttleEnabled>
      <throttleOption>project</throttleOption>
      <limitOneJobWithMatchingParams>false</limitOneJobWithMatchingParams>
      <paramsToUseForLimit />
    </hudson.plugins.throttleconcurrents.ThrottleJobProperty>
  </properties>
  <scm class="hudson.scm.NullSCM" />
  <assignedNode>master</assignedNode>
  <canRoam>false</canRoam>
  <disabled>false</disabled>
  <blockBuildWhenDownstreamBuilding>false</blockBuildWhenDownstreamBuilding>
  <blockBuildWhenUpstreamBuilding>false</blockBuildWhenUpstreamBuilding>
  <triggers />
  <concurrentBuild>true</concurrentBuild>
  <builders>
    <hudson.tasks.Shell>
      <command><!--{%sh-mode%}--><![CDATA[#!/bin/bash
set -x
[%test_command%]
]]><!--{/%sh-mode%}--></command>
    </hudson.tasks.Shell>
  </builders>
  <publishers>
    <hudson.plugins.ws__cleanup.WsCleanup plugin="ws-cleanup@0.34">
      <patterns class="empty-list" />
      <deleteDirs>false</deleteDirs>
      <skipWhenFailed>false</skipWhenFailed>
      <cleanWhenSuccess>true</cleanWhenSuccess>
      <cleanWhenUnstable>true</cleanWhenUnstable>
      <cleanWhenFailure>true</cleanWhenFailure>
      <cleanWhenNotBuilt>true</cleanWhenNotBuilt>
      <cleanWhenAborted>true</cleanWhenAborted>
      <notFailBuild>false</notFailBuild>
      <cleanupMatrixParent>false</cleanupMatrixParent>
      <externalDelete />
    </hudson.plugins.ws__cleanup.WsCleanup>
  </publishers>
  <buildWrappers>
    <hudson.plugins.ws__cleanup.PreBuildCleanup plugin="ws-cleanup@0.34">
      <deleteDirs>false</deleteDirs>
      <cleanupParameter />
      <externalDelete />
    </hudson.plugins.ws__cleanup.PreBuildCleanup>
  </buildWrappers>
</project>

# {%/nxml-mode%}
EOF86c1be707507
           )

    cd $TESTDIR
    echo "${job_xml}" > test-jc.xml

    jc create-job test-jc.xml
    jc build -j test-jc -s -v 2>&1 | tee build.log

    grep -P "^Scheduling project: test-jc" build.log
    grep -P "^building at ${scm_jira_url}job/test-jc/\d+/$" build.log
    grep -P "^Starting building: test-jc #\d+$" build.log
}

test-1-command 'for x in $(seq 1 10); do sleep 1; done'
