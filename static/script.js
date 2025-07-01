document.addEventListener('DOMContentLoaded', () => {
  // Form Validation – only attach this if the element exists.
  const articleForm = document.getElementById('article-form');
  if (articleForm) {
      articleForm.addEventListener('submit', (e) => {
          const selected = document.querySelector('input[name="preference"]:checked');
          if (!selected) {
              alert('Please select a preference before proceeding.');
              e.preventDefault();
          }
      });
  }

  // Info circle and info box toggling
  const infoCircle = document.getElementById('info-circle');
  const infoBox = document.getElementById('info-box');
  const closeInfo = document.getElementById('close-info');
  if (infoCircle && infoBox && closeInfo) {
      infoCircle.addEventListener('click', function() {
          infoBox.style.display = 'block';
      });
      closeInfo.addEventListener('click', function() {
          infoBox.style.display = 'none';
      });
  }
});

// Function to select radio button when clicking on article
function selectVersion(index) {
  const radio = document.getElementById('version' + index);
  if (radio) {
      radio.checked = true;
  }
}

function selectVersion(idx) {
    /* 1 – tick the real radio button (this line was already there) */
    document.getElementById('version' + idx).checked = true;
  
    /* 2 – visual feedback for small screens */
    document.querySelectorAll('.article-version')
            .forEach(el => el.classList.remove('selected'));
    document.querySelectorAll('.article-version')[idx]
            .classList.add('selected');
  }
  
