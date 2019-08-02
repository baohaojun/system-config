#!/usr/bin/env perl

use v5.10;
my $comment = <<~'EOFc8d96c1eb1b8';
# {%org-mode%}
这个脚本实现 3 个应用场景：

1. 在终端上交互式调用
2. 在 jc build 启动一个新的 build 的时候，提前调用
3. 在 jenkins build 已经启动之后，build 脚本自己调用检查
# {%/org-mode%}
EOFc8d96c1eb1b8

use strict;

## start code-generator "^\\s *#\\s *"
# generate-getopt -s perl j:job-name t:gr-topic c:gr-change p:gr-patchset sscan-queues @dry-run=1 @must-compare-patchset=1
## end code-generator
## start generated code
use Getopt::Long;

Getopt::Long::Configure("default");



my $dry_run = 1;
my $gr_change = "";
my $gr_patchset = "";
my $gr_topic = "";
my $job_name = "";
my $must_compare_patchset = 1;
my $scan_queues = 0;

my $handler_help = sub {
  print ;
  print "\n\n选项和参数：\n";
  printf "%6s", '';
  printf "%-24s", '--[no]dry-run';
  if (length('--[no]dry-run') > 24 and length() > 0) {
    print "\n";
    printf "%30s", "";
  }
  printf "%s", ;
  print "\n";
  printf "%6s", '-c, ';
  printf "%-24s", '--gr-change=GR-CHANGE';
  if (length('--gr-change=GR-CHANGE') > 24 and length() > 0) {
    print "\n";
    printf "%30s", "";
  }
  printf "%s", ;
  print "\n";
  printf "%6s", '-p, ';
  printf "%-24s", '--gr-patchset=GR-PATCHSET';
  if (length('--gr-patchset=GR-PATCHSET') > 24 and length() > 0) {
    print "\n";
    printf "%30s", "";
  }
  printf "%s", ;
  print "\n";
  printf "%6s", '-t, ';
  printf "%-24s", '--gr-topic=GR-TOPIC';
  if (length('--gr-topic=GR-TOPIC') > 24 and length() > 0) {
    print "\n";
    printf "%30s", "";
  }
  printf "%s", ;
  print "\n";
  printf "%6s", '-j, ';
  printf "%-24s", '--job-name=JOB-NAME';
  if (length('--job-name=JOB-NAME') > 24 and length() > 0) {
    print "\n";
    printf "%30s", "";
  }
  printf "%s", ;
  print "\n";
  printf "%6s", '';
  printf "%-24s", '--[no]must-compare-patchset';
  if (length('--[no]must-compare-patchset') > 24 and length() > 0) {
    print "\n";
    printf "%30s", "";
  }
  printf "%s", ;
  print "\n";
  printf "%6s", '-s, ';
  printf "%-24s", '--[no]scan-queues';
  if (length('--[no]scan-queues') > 24 and length() > 0) {
    print "\n";
    printf "%30s", "";
  }
  printf "%s", ;
  print "\n";

  exit(0);
};

GetOptions (
            'dry-run!' => \$dry_run,
            'gr-change|c=s' => \$gr_change,
            'gr-patchset|p=s' => \$gr_patchset,
            'gr-topic|t=s' => \$gr_topic,
            'job-name|j=s' => \$job_name,
            'must-compare-patchset!' => \$must_compare_patchset,
            'scan-queues|s!' => \$scan_queues,
            'help|h!' => \&$handler_help,
           );


## end generated code

use JSON;

if (not $job_name) {
  die "Must specify job name";
}

my $builds_json = decode_json(qx(jc get-running-builds));

my %build_url_envmap;

sub should_1_kill_2($$$$) {
  my ($env1, $env2, $url1, $url2) = @_;
  printf STDERR "checking %s t(%s) cp(%d/%d) : %s t(%s) cp(%d/%d)\n",
    $url1, $env1->{GERRIT_TOPIC}, $env1->{GERRIT_CHANGE_NUMBER}, $env1->{GERRIT_PATCHSET_NUMBER},
    $url2, $env2->{GERRIT_TOPIC}, $env2->{GERRIT_CHANGE_NUMBER}, $env2->{GERRIT_PATCHSET_NUMBER};
  if ($env1->{GERRIT_TOPIC} and $env1->{GERRIT_TOPIC} eq $env2->{GERRIT_TOPIC}) {
    say STDERR "Should kill because topic is set and same: $url1 : $url2";
    return 1;
  }

  if ($env1->{GERRIT_TOPIC} or $env2->{GERRIT_TOPIC}) {
    return 0;
  }

  if ($env1->{GERRIT_CHANGE_NUMBER} == $env2->{GERRIT_CHANGE_NUMBER}) {
    if ($env1->{GERRIT_PATCHSET_NUMBER} > $env2->{GERRIT_PATCHSET_NUMBER}) {
      say STDERR "Should kill because patchset number is large: $url1 > $url2";
      return 1;
    } elsif (not $must_compare_patchset) {
      say STDERR "Should kill because don't compare patchset number: $url1 -> $url2";
    }
  }
}

sub kill_builds_for_queue($$) {
  my ($builds, $q_url) = @_;

  my $q_env = decode_json(qx(jc get-queue-envmap -q $q_url));

  for (@{$builds_json}) {
    my $building_job = $_->{job_name};
    next unless $building_job eq $job_name;
    my $build_url = $_->{build_url};

    $build_url_envmap{$build_url} = decode_json(qx(jc get-build-envmap -b $build_url)) unless $build_url_envmap{$build_url};
    my $b_env = $build_url_envmap{$build_url};

    if (should_1_kill_2($q_env, $b_env, $q_url, $build_url)) {
      if ($dry_run) {
        ;
      }
    }
  }

}

if ($scan_queues) {
  my $queue_json = decode_json(qx(jc curl queue/api/json));
  for my $item (@{$queue_json->{items}}) {
    my $task_name = $item->{task}{name};
    next unless $task_name eq $job_name;
    my $params;
    for my $action (@{$item->{actions}}) {
      if ($action->{_class} eq "hudson.model.ParametersAction") {
        $params = $action->{parameters};
        last;
      }
    }

    # 要不要检查一下，当前的这个 queue，如果已经 build 起来了的话，该怎么处理？那种情况下，就当成一个 build 来处理。

    my $q_url = $item->{url};

    my $q_info = decode_json(qx(jc curl ${q_url}api/json));
    my $q_build_url = $q_info->{executable}{url};

    if ($q_build_url) {
      ;
    } else {
      kill_builds_for_queue($builds_json, $q_url);
    }
  }
}
