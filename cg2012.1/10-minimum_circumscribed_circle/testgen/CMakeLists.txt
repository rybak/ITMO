

<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
  <title>
  Rabodaber / evaluator-tasks / source &mdash; Bitbucket
</title>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
  <meta name="description" content="" />
  <meta name="keywords" content="" />
  <!--[if lt IE 9]>
  <script src="https://dwz7u9t8u8usb.cloudfront.net/m/e14058e2cf3f/js/lib/html5.js"></script>
  <![endif]-->

  <script>
    (function (window) {
      // prevent stray occurrences of `console.log` from causing errors in IE
      var console = window.console || (window.console = {});
      console.log || (console.log = function () {});

      var BB = window.BB || (window.BB = {});
      BB.debug = false;
      BB.cname = false;
      BB.CANON_URL = 'https://bitbucket.org';
      BB.MEDIA_URL = 'https://dwz7u9t8u8usb.cloudfront.net/m/e14058e2cf3f/';
      BB.images = {
        noAvatar: 'https://dwz7u9t8u8usb.cloudfront.net/m/e14058e2cf3f/img/no_avatar.png'
      };
      BB.user = {"username": "Rabodaber", "isSshEnabled": false, "isKbdShortcutsEnabled": true, "follows": {"repos": []}, "id": 338581};
      BB.user.has = (function () {
        var betaFeatures = [];
        betaFeatures.push('repo2');
        return function (feature) {
          return _.contains(betaFeatures, feature);
        };
      }());
      BB.targetUser = BB.user;
  
      BB.repo || (BB.repo = {});
  
      BB.user.isAdmin = true;
      BB.repo.id = 706144;
    
    
      BB.repo.language = null;
      BB.repo.pygmentsLanguage = null;
    
    
      BB.repo.slug = 'evaluator\u002Dtasks';
    
    
      BB.repo.owner = {
        username: 'Rabodaber'
      };
    
      // Coerce `BB.repo` to a string to get
      // "davidchambers/mango" or whatever.
      BB.repo.toString = function () {
        return BB.cname ? this.slug : this.owner.username + '/' + this.slug;
      }
    
      BB.repo.parent = {
        slug: 'evaluator\u002Dtasks',
        owner: {
          username: 'assaron'
        },
        toString: BB.repo.toString
      }
    
    
      BB.changeset = '008f65f6c9e6'
    
    
  
    }(this));
  </script>

  


  <link rel="stylesheet" href="https://dwz7u9t8u8usb.cloudfront.net/m/e14058e2cf3f/bun/css/bundle.css"/>



  <link rel="search" type="application/opensearchdescription+xml" href="/opensearch.xml" title="Bitbucket" />
  <link rel="icon" href="https://dwz7u9t8u8usb.cloudfront.net/m/e14058e2cf3f/img/logo_new.png" type="image/png" />
  <link type="text/plain" rel="author" href="/humans.txt" />


  
    <script src="https://dwz7u9t8u8usb.cloudfront.net/m/e14058e2cf3f/bun/js/bundle.js"></script>
  



</head>

<body id="" class="">
  <script>
    if (navigator.userAgent.indexOf(' AppleWebKit/') === -1) {
      $('body').addClass('non-webkit')
    }
    $('body')
      .addClass($.client.os.toLowerCase())
      .addClass($.client.browser.toLowerCase())
  </script>
  <!--[if IE 8]>
  <script>jQuery(document.body).addClass('ie8')</script>
  <![endif]-->
  <!--[if IE 9]>
  <script>jQuery(document.body).addClass('ie9')</script>
  <![endif]-->

  <div id="wrapper">



  <div id="header-wrap">
    <div id="header">
    <ul id="global-nav">
      <li><a class="home" href="http://www.atlassian.com">Atlassian Home</a></li>
      <li><a class="docs" href="http://confluence.atlassian.com/display/BITBUCKET">Documentation</a></li>
      <li><a class="support" href="/support">Support</a></li>
      <li><a class="blog" href="http://blog.bitbucket.org">Blog</a></li>
      <li><a class="forums" href="http://groups.google.com/group/bitbucket-users">Forums</a></li>
    </ul>
    <a href="/" id="logo">Bitbucket by Atlassian</a>

    <div id="main-nav">
    

      <ul class="clearfix">
        <li><a href="/explore" id="explore-link">Explore</a></li>
        <li><a href="https://bitbucket.org" id="dashboard-link">Dashboard</a></li>
        <li id="repositories-dropdown" class="inertial-hover active">
          <a class="drop-arrow" href="/repo/mine" id="repositories-link">Repositories</a>
          <div>
            <div>
              <div id="repo-overview"></div>
              <div class="group">
                <a href="/repo/create" class="new-repository" id="create-repo-link">Create repository</a>
                <a href="/repo/import" class="import-repository" id="import-repo-link">Import repository</a>
              </div>
            </div>
          </div>
        </li>
        <li id="user-dropdown" class="inertial-hover">
          <a class="drop-arrow" href="/Rabodaber">
            <span>Андрей Васин</span>
          </a>
          <div>
            <div>
              <div class="group">
                <a href="/account/user/Rabodaber/" id="account-link">Account</a>
                <a href="/account/notifications/" id="inbox-link">Inbox <span>(5)</span></a>
              </div>

      

              <div class="group">
                <a href="/account/signout/">Log out</a>
              </div>
            </div>
          </div>
        </li>
        

<li class="search-box">
  
    <form action="/repo/all">
      <input type="search" results="5" autosave="bitbucket-explore-search"
             name="name" id="searchbox"
             placeholder="owner/repo" />
  
  </form>
</li>

      </ul>

    
    </div>

  

    </div>
  </div>

    <div id="header-messages">
  
  
    
    
    
    
  

    
   </div>



    <div id="content">
      <div id="source">
      
  
  





  <script>
    jQuery(function ($) {
        var cookie = $.cookie,
            cookieOptions, date,
            $content = $('#content'),
            $pane = $('#what-is-bitbucket'),
            $hide = $pane.find('[href="#hide"]').css('display', 'block').hide();

        date = new Date();
        date.setTime(date.getTime() + 365 * 24 * 60 * 60 * 1000);
        cookieOptions = { path: '/', expires: date };

        if (cookie('toggle_status') == 'hide') $content.addClass('repo-desc-hidden');

        $('#toggle-repo-content').click(function (event) {
            event.preventDefault();
            $content.toggleClass('repo-desc-hidden');
            cookie('toggle_status', cookie('toggle_status') == 'show' ? 'hide' : 'show', cookieOptions);
        });

        if (!cookie('hide_intro_message')) $pane.show();

        $hide.click(function (event) {
            event.preventDefault();
            cookie('hide_intro_message', true, cookieOptions);
            $pane.slideUp('slow');
        });

        $pane.hover(
            function () { $hide.fadeIn('fast'); },
            function () { $hide.fadeOut('fast'); });

      (function () {
        // Update "recently-viewed-repos" cookie for
        // the "repositories" drop-down.
        var
          id = BB.repo.id,
          cookieName = 'recently-viewed-repos_' + BB.user.id,
          rvr = cookie(cookieName),
          ids = rvr? rvr.split(','): [],
          idx = _.indexOf(ids, '' + id);

        // Remove `id` from `ids` if present.
        if (~idx) ids.splice(idx, 1);

        cookie(
          cookieName,
          // Insert `id` as the first item, then call
          // `join` on the resulting array to produce
          // something like "114694,27542,89002,84570".
          [id].concat(ids.slice(0, 4)).join(),
          {path: '/', expires: 1e6} // "never" expires
        );
      }());
    });
  </script>




<div id="tabs" class="tabs">
  <ul>
    <li>
      <a href="/Rabodaber/evaluator-tasks/overview" id="repo-overview-link">Overview</a>
    </li>

    <li>
      <a href="/Rabodaber/evaluator-tasks/downloads" id="repo-downloads-link">Downloads (<span id="downloads-count">0</span>)</a>
    </li>

    

    <li>
      <a href="/Rabodaber/evaluator-tasks/pull-requests" id="repo-pr-link">Pull requests (0)</a>
    </li>

    <li class="selected">
      
        <a href="/Rabodaber/evaluator-tasks/src" id="repo-source-link">Source</a>
      
    </li>

    <li>
      <a href="/Rabodaber/evaluator-tasks/changesets" id="repo-commits-link">Commits</a>
    </li>

    <li id="wiki-tab" class="dropdown"
      style="display:
          block 
        
      ">
      <a href="/Rabodaber/evaluator-tasks/wiki" id="repo-wiki-link">Wiki</a>
    </li>

    <li id="issues-tab" class="dropdown inertial-hover"
      style="display:
        block 
        
      ">
      <a href="/Rabodaber/evaluator-tasks/issues?status=new&amp;status=open" id="repo-issues-link">Issues (0) &raquo;</a>
      <ul>
        <li><a href="/Rabodaber/evaluator-tasks/issues/new">Create new issue</a></li>
        <li><a href="/Rabodaber/evaluator-tasks/issues?status=new">New issues</a></li>
        <li><a href="/Rabodaber/evaluator-tasks/issues?status=new&amp;status=open">Open issues</a></li>
        <li><a href="/Rabodaber/evaluator-tasks/issues?status=duplicate&amp;status=invalid&amp;status=resolved&amp;status=wontfix">Closed issues</a></li>
        
          <li><a href="/Rabodaber/evaluator-tasks/issues?responsible=Rabodaber">My issues</a></li>
        
        <li><a href="/Rabodaber/evaluator-tasks/issues">All issues</a></li>
        <li><a href="/Rabodaber/evaluator-tasks/issues/query">Advanced query</a></li>
      </ul>
    </li>

    
      <li>
        <a href="/Rabodaber/evaluator-tasks/admin" id="repo-admin-link">Admin</a>
      </li>
    
  </ul>

  <ul>
    <li>
      <a href="/Rabodaber/evaluator-tasks/descendants" id="repo-forks-link">Forks/queues (0)</a>
    </li>

    <li>
      <a href="/Rabodaber/evaluator-tasks/zealots">Followers (<span id="followers-count">2</span>)</a>
    </li>
  </ul>
</div>




  <div id="invitation-dialog" title="Send an invitation">

<form class="invitation-form newform"
  method="post"
  action="/api/1.0/invitations/Rabodaber/evaluator-tasks"
  novalidate>
  <div style='display:none'><input type='hidden' name='csrfmiddlewaretoken' value='634647515f404706dd3c565e7fe2b35f' /></div>
  <div class="error_ message_"><h4></h4></div>
  <div class="success_ message_"><h4></h4></div>
  <label for="id-email-address">Email address</label>
  <input type="email" id="id-email-address" name="email-address">
  <select name="permission" class="nosearch">
  
    <option value="write">Write access</option>
    <option value="admin">Admin access</option>
  </select>
  <input type="submit" value="Send invitation" />
</form>
</div>
 

  <div class="repo-menu" id="repo-menu">
    <ul id="repo-menu-links">
    
      <li>
        <a id="repo-invite-link" href="#share" class="share">invite</a>
      </li>
    
      <li>
        <a href="/Rabodaber/evaluator-tasks/rss?token=d260a7d73f188c47ec5a30274652b22e" class="rss" title="RSS feed for evaluator-tasks">RSS</a>
      </li>

      <li><a id="repo-fork-link" href="/Rabodaber/evaluator-tasks/fork" class="fork">fork</a></li>
      
        
          <li><a id="repo-patch-queue-link" href="/Rabodaber/evaluator-tasks/hack" class="patch-queue">patch queue</a></li>
        
      
      <li>
        <a id="repo-follow-link" rel="nofollow" href="/Rabodaber/evaluator-tasks/follow" class="follow">follow</a>
      </li>
      
          
      
      
        <li class="get-source inertial-hover">
          <a class="source">get source</a>
          <ul class="downloads">
            
              
              <li><a rel="nofollow" href="/Rabodaber/evaluator-tasks/get/008f65f6c9e6.zip">zip</a></li>
              <li><a rel="nofollow" href="/Rabodaber/evaluator-tasks/get/008f65f6c9e6.tar.gz">gz</a></li>
              <li><a rel="nofollow" href="/Rabodaber/evaluator-tasks/get/008f65f6c9e6.tar.bz2">bz2</a></li>
            
          </ul>
        </li>
      
    </ul>

  
    <ul class="metadata">
      
      
        <li class="branches inertial-hover">branches
          <ul>
            <li><a href="/Rabodaber/evaluator-tasks/src/008f65f6c9e6" title="default">default</a>
              
              
            </li>
          </ul>
        </li>
      
      
      <li class="tags inertial-hover">tags
        <ul>
          <li><a href="/Rabodaber/evaluator-tasks/src/008f65f6c9e6">tip</a>
            
            </li>
        </ul>
      </li>
     
     
    </ul>
  
</div>

<div class="repo-menu" id="repo-desc">
    <ul id="repo-menu-links-mini">
      
        <li><a rel="nofollow" class="compare-link"
               href="/Rabodaber/evaluator-tasks/compare/..assaron/evaluator-tasks"
               title="Show changes between evaluator-tasks and evaluator-tasks"
               ></a></li>
        
          <li>
            <a href="/Rabodaber/evaluator-tasks/pull-request/new" class="pull-request" title="Pull request"></a>
          </li>
        
      

      
        <li>
          <a href="#share" class="share" title="Invite">invite</a>
        </li>
      
      <li>
        <a href="/Rabodaber/evaluator-tasks/rss?token=d260a7d73f188c47ec5a30274652b22e" class="rss" title="RSS feed for evaluator-tasks"></a>
      </li>

      <li><a href="/Rabodaber/evaluator-tasks/fork" class="fork" title="Fork"></a></li>
      
        
          <li><a href="/Rabodaber/evaluator-tasks/hack" class="patch-queue" title="Patch queue"></a></li>
        
      
      <li>
        <a rel="nofollow" href="/Rabodaber/evaluator-tasks/follow" class="follow">follow</a>
      </li>
      
          
      
      
        <li>
          <a class="source" title="Get source"></a>
          <ul class="downloads">
            
              
              <li><a rel="nofollow" href="/Rabodaber/evaluator-tasks/get/008f65f6c9e6.zip">zip</a></li>
              <li><a rel="nofollow" href="/Rabodaber/evaluator-tasks/get/008f65f6c9e6.tar.gz">gz</a></li>
              <li><a rel="nofollow" href="/Rabodaber/evaluator-tasks/get/008f65f6c9e6.tar.bz2">bz2</a></li>
            
          </ul>
        </li>
      
    </ul>

    <h3 id="repo-heading" class="public hg">
      <a class="owner-username" href="/Rabodaber">Rabodaber</a> /
      <a class="repo-name" href="/Rabodaber/evaluator-tasks">evaluator-tasks</a>
    
      (fork of
      
        <a href="/assaron">assaron</a> /
      
      <a href="/assaron/evaluator-tasks">evaluator-tasks</a>)
    

    
      <ul id="fork-actions" class="button-group">
      
        <li>
          <a id="repo-compare-link" href="/Rabodaber/evaluator-tasks/compare/..assaron/evaluator-tasks"
             rel="nofollow" class="icon compare-link">compare fork</a>
        </li>
      
      
        <li>
          <a id="repo-create-pr-link" href="/Rabodaber/evaluator-tasks/pull-request/new"
             class="icon pull-request">create pull request</a>
        </li>
      
      </ul>
    
    </h3>

    

  <div id="repo-desc-cloneinfo">Clone this repository (size: 28.2 KB):
    <a href="https://bitbucket.org/Rabodaber/evaluator-tasks" class="https">HTTPS</a> /
    <a href="ssh://hg@bitbucket.org/Rabodaber/evaluator-tasks" class="ssh">SSH</a>
    <pre id="clone-url-https">hg clone https://bitbucket.org/Rabodaber/evaluator-tasks</pre>
    <pre id="clone-url-ssh">hg clone ssh://hg@bitbucket.org/Rabodaber/evaluator-tasks</pre>
    
  </div>

        <a href="#" id="toggle-repo-content"></a>

        

</div>




      
  <div id="source-container">
    

  <div id="source-path">
    <h1>
      <a href="/Rabodaber/evaluator-tasks/src" class="src-pjax">evaluator-tasks</a> /

  
    
      <a href="/Rabodaber/evaluator-tasks/src/008f65f6c9e6/cg2012.1/" class="src-pjax">cg2012.1</a> /
    
  

  
    
      <a href="/Rabodaber/evaluator-tasks/src/008f65f6c9e6/cg2012.1/10-minimum_circumscribed_circle/" class="src-pjax">10-minimum_circumscribed_circle</a> /
    
  

  
    
      <a href="/Rabodaber/evaluator-tasks/src/008f65f6c9e6/cg2012.1/10-minimum_circumscribed_circle/testgen/" class="src-pjax">testgen</a> /
    
  

  
    
      <span>CMakeLists.txt</span>
    
  

    </h1>
  </div>

  <div class="labels labels-csv">
  
    <dl>
  
    
  
  
    
  
  
    <dt>Branch</dt>
    
      
        <dd class="branch unabridged"><a href="/Rabodaber/evaluator-tasks/changesets/tip/branch(%22default%22)" title="default">default</a></dd>
      
    
  
</dl>

  
  </div>


  
  <div id="source-view">
    <div class="header">
      <ul class="metadata">
        <li><code>008f65f6c9e6</code></li>
        
          
            <li>7 loc</li>
          
        
        <li>278 bytes</li>
      </ul>
      <ul class="source-view-links">
        
        <li><a id="embed-link" href="https://bitbucket.org/Rabodaber/evaluator-tasks/src/008f65f6c9e6/cg2012.1/10-minimum_circumscribed_circle/testgen/CMakeLists.txt?embed=t">embed</a></li>
        
        <li><a href="/Rabodaber/evaluator-tasks/history/cg2012.1/10-minimum_circumscribed_circle/testgen/CMakeLists.txt">history</a></li>
        
        <li><a href="/Rabodaber/evaluator-tasks/annotate/008f65f6c9e6/cg2012.1/10-minimum_circumscribed_circle/testgen/CMakeLists.txt">annotate</a></li>
        
        <li><a href="/Rabodaber/evaluator-tasks/raw/008f65f6c9e6/cg2012.1/10-minimum_circumscribed_circle/testgen/CMakeLists.txt">raw</a></li>
        <li>
          <form action="/Rabodaber/evaluator-tasks/diff/cg2012.1/10-minimum_circumscribed_circle/testgen/CMakeLists.txt" class="source-view-form">
          
            <input type="hidden" name="diff2" value="008f65f6c9e6" />
            <select name="diff1">
            
              
            
            </select>
            <input type="submit" value="diff" />
          
          </form>
        </li>
      </ul>
    </div>
  
    <div>
    <table class="highlighttable"><tr><td class="linenos"><div class="linenodiv"><pre><a href="#cl-1">1</a>
<a href="#cl-2">2</a>
<a href="#cl-3">3</a>
<a href="#cl-4">4</a>
<a href="#cl-5">5</a>
<a href="#cl-6">6</a>
<a href="#cl-7">7</a>
</pre></div></td><td class="code"><div class="highlight"><pre><a name="cl-1"></a>cmake_minimum_required(VERSION 2.6)
<a name="cl-2"></a>
<a name="cl-3"></a>project(minimumcircumscribedcircle-generator)
<a name="cl-4"></a>set(sources correctness_generator.cpp perfomance_generator.cpp)
<a name="cl-5"></a>add_definitions(-O2 -frounding-math)
<a name="cl-6"></a>link_libraries(CGAL gmp)
<a name="cl-7"></a>add_executable(minimumcircumscribedcircle-generator ${sources})
</pre></div>
</td></tr></table>
    </div>
  
  </div>
  


  <div id="mask"><div></div></div>

  </div>

      </div>
    </div>

  </div>

  <div id="footer">
    <ul id="footer-nav">
      <li>Copyright © 2012 <a href="http://atlassian.com">Atlassian</a></li>
      <li><a href="http://www.atlassian.com/hosted/terms.jsp">Terms of Service</a></li>
      <li><a href="http://www.atlassian.com/about/privacy.jsp">Privacy</a></li>
      <li><a href="//bitbucket.org/site/master/issues/new">Report a Bug to Bitbucket</a></li>
      <li><a href="http://confluence.atlassian.com/x/IYBGDQ">API</a></li>
      <li><a href="http://status.bitbucket.org/">Server Status</a></li>
    </ul>
    <ul id="social-nav">
      <li class="blog"><a href="http://blog.bitbucket.org">Bitbucket Blog</a></li>
      <li class="twitter"><a href="http://www.twitter.com/bitbucket">Twitter</a></li>
    </ul>
    <h5>We run</h5>
    <ul id="technologies">
      <li><a href="http://www.djangoproject.com/">Django 1.3.1</a></li>
      <li><a href="//bitbucket.org/jespern/django-piston/">Piston 0.3dev</a></li>
      <li><a href="http://git-scm.com/">Git 1.7.6</a></li>
      <li><a href="http://www.selenic.com/mercurial/">Hg 1.9.1</a></li>
      <li><a href="http://www.python.org">Python 2.7.2</a></li>
      <li>15abb9492b7d | bitbucket05</li>
    </ul>
  </div>

  <script src="https://dwz7u9t8u8usb.cloudfront.net/m/e14058e2cf3f/js/lib/global.js"></script>






  <script>
    BB.gaqPush(['_trackPageview']);
  
    BB.gaqPush(['atl._trackPageview']);

    

    

    (function () {
        var ga = document.createElement('script');
        ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
        ga.setAttribute('async', 'true');
        document.documentElement.firstChild.appendChild(ga);
    }());
  </script>

</body>
</html>
