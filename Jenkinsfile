pipeline {
    agent {
        label {
          label "woha"
          customWorkspace "here"
        }
    }
    stages {
        stage('build') {
            steps {
                sh 'python --version'
            }
        }
    }
}
