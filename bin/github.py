"""This script is github cli for projects and stuff, that uses the REST api."""

import click
from github import Github
from github import Project
from github import GithubObject
from github import Repository
import os
import subprocess
import shlex
import re


@click.group()
@click.option('--debug/--no-debug', default=False)
@click.option('--username', envvar='GITHUB_USERNAME')
@click.option('--access-token', envvar='GITHUB_TOKEN')
@click.pass_context
def github(ctx, debug, username, access_token):
    ctx.ensure_object(dict)
    ctx.obj['DEBUG'] = debug
    ctx.obj['access_token'] = access_token
    g = ctx.obj['g'] = Github(ctx.obj['access_token'])
    user = ctx.obj['user'] = g.get_user()
    username = ctx.obj['username'] = username
    repos = ctx.obj['repos'] = user.get_repos()


@github.command()
@click.pass_context
@click.option('--repo-name')
@click.option('--private/--public', default=True)
def create(ctx, repo_name, private=True):
    user = ctx.obj['user']
    repos = ctx.obj['repos']

    if not any([x.name == repo_name for x in repos]):
        user.create_repo(repo_name, private=private)


@github.command()
@click.pass_context
@click.option('--branch-topic', '-b', help='topic branch 的名字')
@click.option('--commit-msg', '-m', help='已修改的仓库的 commit msg')
@click.option('--github-url', '-u', envvar='GITHUB_PUSH_URL')
def save_to_github(ctx, branch_topic, commit_msg, github_url):
    g = ctx.obj['g']
    user = ctx.obj['user']
    username = ctx.obj['username']

    if not github_url:
        github_url = ("%s.github" % username)


    def save_1_dir(dir):
        click.echo(f'working with {dir}')
        os.chdir(dir)
        project = subprocess.check_output('bp $(c -e PWD repo-project)', shell=True).rstrip().decode('UTF-8')
        project = re.compile("\\.git$").sub("", project)
        ctx.invoke(create, repo_name=project)
        os.environ['commit_msg'] = commit_msg
        os.environ['branch_topic'] = branch_topic
        os.environ['repo_project'] = project
        os.environ['github_url'] = github_url
        os.environ['username'] = username

        shell_command = """
        set -x
        save.-to-git -m "${commit_msg:-$branch_topic}"
        git push ${github_url}:${username}/${repo_project} HEAD:refs/heads/topic/${branch_topic}
        """
        subprocess.check_call(shell_command, shell=True)

    if (os.path.exists(".repo/")):
        dirs = subprocess.check_output(["repo", "forall", "-c", 'pwd', ]).rstrip().decode('UTF-8')
        for d in dirs.split("\n"):
            if os.path.exists(d):
                save_1_dir(d)
    else:
        save_1_dir(os.getcwd())

if __name__ == '__main__':
    github(obj={})
